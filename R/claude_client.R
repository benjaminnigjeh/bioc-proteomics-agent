# R/claude_client.R
#
# Thin, defensive client for the Claude Messages API (tool use). Implements
# the same plan/next-action interface as R/mock_llm.R so agent_execution.R
# can treat both backends identically. The API key is read once from the
# environment via get_app_config() and is never logged, echoed, or placed
# in any object that reaches the browser.
#
# Reference: https://docs.claude.com/en/api/messages

ANTHROPIC_API_URL <- "https://api.anthropic.com/v1/messages"
ANTHROPIC_API_VERSION <- "2023-06-01"

#' Structured error helper. `type` is one of:
#'  "not_configured", "auth_error", "model_unavailable", "rate_limit",
#'  "timeout", "malformed_tool_arguments", "max_steps_reached",
#'  "network_error", "unknown_error"
.claude_error <- function(type, message) {
  structure(list(type = type, message = message), class = "claude_error")
}

#' Classify an httr2 HTTP error response into a guardrail-friendly type.
.classify_http_error <- function(resp) {
  status <- httr2::resp_status(resp)
  body <- tryCatch(httr2::resp_body_json(resp, check_type = FALSE), error = function(e) list())
  api_msg <- tryCatch(body$error$message, error = function(e) NULL) %||% "Unknown API error."
  if (status == 401) return(.claude_error("auth_error", paste("Authentication failed:", api_msg)))
  if (status == 404) return(.claude_error("model_unavailable", paste("Model unavailable:", api_msg)))
  if (status == 429) return(.claude_error("rate_limit", paste("Rate limit exceeded:", api_msg)))
  if (status >= 500) return(.claude_error("network_error", paste("Anthropic service error:", api_msg)))
  .claude_error("unknown_error", sprintf("Claude API error (HTTP %s): %s", status, api_msg))
}

#' Low-level call to the Messages API with retry-on-transient-failure only.
#' Never retries auth errors, malformed requests, or 4xx client errors
#' other than 429.
#'
#' @export
claude_messages_call <- function(messages, tools = NULL, system = NULL, cfg) {
  if (!claude_is_configured(cfg)) {
    return(list(ok = FALSE, error = .claude_error("not_configured",
      "ANTHROPIC_API_KEY is not set. Switch to LLM_MODE=mock or provide a key in .env.")))
  }
  if (!requireNamespace("httr2", quietly = TRUE)) {
    return(list(ok = FALSE, error = .claude_error("unknown_error", "The 'httr2' package is required for Claude mode.")))
  }

  body <- list(
    model = cfg$anthropic_model,
    max_tokens = 4096,
    messages = messages
  )
  if (!is.null(system)) body$system <- system
  if (!is.null(tools)) body$tools <- tools

  req <- httr2::request(ANTHROPIC_API_URL) |>
    httr2::req_headers(
      "x-api-key" = cfg$anthropic_api_key,
      "anthropic-version" = ANTHROPIC_API_VERSION,
      "content-type" = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(cfg$claude_request_timeout) |>
    httr2::req_error(is_error = function(resp) FALSE)

  attempt <- 0
  repeat {
    attempt <- attempt + 1
    resp <- tryCatch(httr2::req_perform(req), error = function(e) e)

    if (inherits(resp, "error") || inherits(resp, "condition")) {
      msg <- conditionMessage(resp)
      is_timeout <- grepl("timed out|timeout", msg, ignore.case = TRUE)
      err <- if (is_timeout) {
        .claude_error("timeout", sprintf("Claude API request timed out after %ss.", cfg$claude_request_timeout))
      } else {
        .claude_error("network_error", paste("Network error contacting Claude API:", msg))
      }
      if (attempt <= cfg$max_tool_retries && err$type %in% c("timeout", "network_error")) {
        Sys.sleep(min(2^attempt, 10))
        next
      }
      return(list(ok = FALSE, error = err))
    }

    status <- httr2::resp_status(resp)
    if (status >= 200 && status < 300) {
      parsed <- tryCatch(httr2::resp_body_json(resp, check_type = FALSE), error = function(e) NULL)
      if (is.null(parsed)) {
        return(list(ok = FALSE, error = .claude_error("malformed_tool_arguments", "Claude returned a response body that could not be parsed as JSON.")))
      }
      return(list(ok = TRUE, response = parsed))
    }

    err <- .classify_http_error(resp)
    retryable <- err$type %in% c("rate_limit", "network_error")
    if (retryable && attempt <= cfg$max_tool_retries) {
      Sys.sleep(min(2^attempt, 10))
      next
    }
    return(list(ok = FALSE, error = err))
  }
}

#' Ask Claude for a structured, human-readable multi-step plan grounded in
#' the current session's available data. No tools are exposed for this
#' call -- planning is separated from execution so the user can review the
#' plan before anything runs.
#'
#' @export
claude_generate_plan <- function(objective, available, agent_descriptions, cfg) {
  system_prompt <- paste0(
    "You are the Supervisor Agent of a Bioconductor mass-spectrometry proteomics assistant. ",
    "You NEVER calculate scientific results yourself and you NEVER write or execute code. ",
    "You design a short, numbered plan that names, for each step, which specialist agent and ",
    "which registered tool would be used and why. Available specialist agents:\n",
    paste(sprintf("- %s: %s", names(agent_descriptions), unlist(agent_descriptions)), collapse = "\n"),
    "\n\nRespond with a concise numbered plan only (5-10 steps). Do not invent tools or data that are not described below."
  )
  user_content <- paste0(
    "Objective: ", objective, "\n\n",
    "Session context (already available, do not re-derive):\n",
    jsonlite::toJSON(available, auto_unbox = TRUE, null = "null")
  )
  res <- claude_messages_call(
    messages = list(list(role = "user", content = user_content)),
    tools = NULL,
    system = system_prompt,
    cfg = cfg
  )
  if (!res$ok) return(list(ok = FALSE, error = res$error))

  text_blocks <- Filter(function(b) identical(b$type, "text"), res$response$content %||% list())
  plan_text <- paste(vapply(text_blocks, function(b) b$text, character(1)), collapse = "\n")
  if (!nzchar(plan_text)) {
    return(list(ok = FALSE, error = .claude_error("malformed_tool_arguments", "Claude's plan response contained no text content.")))
  }
  list(ok = TRUE, plan_text = plan_text, source = "claude")
}

#' Advance the bounded tool-use loop by one Claude turn. `state$messages`
#' holds the running Messages-API conversation (already includes prior
#' tool_result blocks appended by agent_execution.R). Returns:
#'   list(ok, type = "tool_use", tool, args, tool_use_id, reason)
#'   list(ok, type = "final", text)
#'   list(ok = FALSE, error)
#'
#' @export
claude_next_action <- function(state, tools_meta, cfg) {
  system_prompt <- state$system_prompt
  res <- claude_messages_call(
    messages = state$messages,
    tools = tools_meta,
    system = system_prompt,
    cfg = cfg
  )
  if (!res$ok) return(list(ok = FALSE, error = res$error))

  content <- res$response$content %||% list()
  tool_blocks <- Filter(function(b) identical(b$type, "tool_use"), content)
  text_blocks <- Filter(function(b) identical(b$type, "text"), content)
  narrative <- paste(vapply(text_blocks, function(b) b$text, character(1)), collapse = "\n")

  if (length(tool_blocks) > 0) {
    tb <- tool_blocks[[1]]
    if (is.null(tb$name) || is.null(tb$input)) {
      return(list(ok = FALSE, error = .claude_error("malformed_tool_arguments",
        "Claude requested a tool call missing a name or input arguments.")))
    }
    return(list(ok = TRUE, type = "tool_use", tool = tb$name, args = tb$input,
                tool_use_id = tb$id, reason = if (nzchar(narrative)) narrative else "Claude selected this tool based on the plan and prior results.",
                assistant_content = content))
  }

  if (nzchar(narrative)) {
    return(list(ok = TRUE, type = "final", text = narrative))
  }
  list(ok = FALSE, error = .claude_error("malformed_tool_arguments", "Claude's response contained neither a tool call nor text."))
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
