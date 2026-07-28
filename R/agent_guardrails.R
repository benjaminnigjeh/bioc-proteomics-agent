# R/agent_guardrails.R
#
# The Guardrail Agent. This is the single choke point between "Claude
# requested tool X with arguments Y" and "R function X actually runs".
# Nothing in this file ever evaluates Claude-authored code or strings.

# Tool names that must never exist in the registry. Checked defensively at
# startup in case a future contributor registers something dangerous.
FORBIDDEN_TOOL_NAME_PATTERNS <- c(
  "eval", "parse", "system", "shell", "exec", "source", "install", "download",
  "readLines_env", "Sys.setenv", "Sys.getenv", "file.remove", "unlink_all"
)

#' Run at startup: fail loudly if a forbidden-shaped tool was registered.
#' @export
assert_registry_is_safe <- function() {
  bad <- character(0)
  for (nm in list_tool_names()) {
    for (pat in FORBIDDEN_TOOL_NAME_PATTERNS) {
      if (grepl(pat, nm, ignore.case = TRUE)) bad <- c(bad, nm)
    }
  }
  if (length(bad) > 0) {
    stop("Guardrail violation: forbidden tool name(s) registered: ", paste(unique(bad), collapse = ", "))
  }
  invisible(TRUE)
}

#' Minimal structural JSON-schema check: required fields present, and for
#' properties with declared type/enum/minimum/maximum, a best-effort check.
#' This is intentionally conservative -- it never coerces types beyond
#' simple numeric coercion, and never executes attacker-controlled code.
#'
#' @export
schema_check <- function(args, schema) {
  errors <- character(0)
  args <- args %||% list()
  required <- unlist(schema$required %||% list())
  for (r in required) {
    v <- args[[r]]
    # The empty-string check only makes sense for scalar string arguments;
    # array-typed required arguments (e.g. sample_cols) are legitimately
    # length > 1, and must not be run through nzchar()/&& as if scalar.
    is_empty_string <- is.character(v) && length(v) == 1 && !nzchar(v)
    if (is.null(v) || is_empty_string) {
      errors <- c(errors, sprintf("Missing required argument '%s'.", r))
    }
  }
  props <- schema$properties %||% list()
  for (nm in names(args)) {
    if (!nm %in% names(props)) {
      errors <- c(errors, sprintf("Unknown argument '%s' is not permitted by the tool schema.", nm))
      next
    }
    p <- props[[nm]]
    v <- args[[nm]]
    if (is.null(v)) next
    if (!is.null(p$type)) {
      ok_type <- switch(p$type,
        string  = is.character(v),
        number  = is.numeric(v),
        integer = is.numeric(v) && all(v == round(v)),
        boolean = is.logical(v),
        array   = is.list(v) || is.vector(v),
        object  = is.list(v),
        TRUE
      )
      if (isFALSE(ok_type)) errors <- c(errors, sprintf("Argument '%s' has the wrong type (expected %s).", nm, p$type))
    }
    if (!is.null(p$enum) && !is.null(v)) {
      allowed <- unlist(p$enum)
      if (!(v %in% allowed)) errors <- c(errors, sprintf("Argument '%s' must be one of: %s.", nm, paste(allowed, collapse = ", ")))
    }
    if (!is.null(p$minimum) && is.numeric(v) && any(v < p$minimum)) {
      errors <- c(errors, sprintf("Argument '%s' must be >= %s.", nm, p$minimum))
    }
    if (!is.null(p$maximum) && is.numeric(v) && any(v > p$maximum)) {
      errors <- c(errors, sprintf("Argument '%s' must be <= %s.", nm, p$maximum))
    }
  }
  list(ok = length(errors) == 0, errors = errors)
}

#' Full guardrail validation of a requested tool call: existence, schema,
#' domain-specific validation. Does NOT execute anything.
#'
#' @export
guardrail_validate_call <- function(tool_name, args, ctx) {
  if (is.null(tool_name) || !nzchar(tool_name)) {
    return(list(ok = FALSE, errors = "No tool name supplied.", tool = NULL, args = args))
  }
  tool <- get_tool(tool_name)
  if (is.null(tool)) {
    return(list(ok = FALSE, errors = sprintf("Tool '%s' is not registered. Only allowlisted tools may be called.", tool_name),
                tool = NULL, args = args))
  }
  sc <- schema_check(args, tool$input_schema)
  if (!sc$ok) {
    return(list(ok = FALSE, errors = paste(sc$errors, collapse = " "), tool = tool, args = args))
  }
  dv <- tool$validate(args, ctx)
  if (!isTRUE(dv$ok)) {
    return(list(ok = FALSE, errors = paste(dv$errors, collapse = " "), tool = tool, args = args))
  }
  list(ok = TRUE, errors = character(0), tool = tool, args = dv$args %||% args)
}

#' Execute a tool that has already passed guardrail_validate_call(), with a
#' soft timeout and duration measurement. Returns a uniform result envelope.
#'
#' @export
guardrail_execute_call <- function(tool, args, ctx) {
  start <- Sys.time()
  result <- tryCatch({
    if (requireNamespace("R.utils", quietly = TRUE)) {
      R.utils::withTimeout({
        tool$execute(args, ctx)
      }, timeout = tool$timeout_s, onTimeout = "error")
    } else {
      tool$execute(args, ctx)
    }
  }, error = function(e) {
    list(.error = conditionMessage(e))
  })
  duration <- as.numeric(difftime(Sys.time(), start, units = "secs"))

  if (!is.null(result$.error)) {
    return(list(ok = FALSE, error = redact_secrets(result$.error), duration_s = duration))
  }
  list(ok = TRUE, output = result$output, output_id = result$output_id %||% NA_character_,
       artifacts = result$artifacts %||% character(0), warnings = result$warnings %||% character(0),
       duration_s = duration)
}

#' Best-effort check that a Claude-authored narrative's numeric claims are
#' grounded in the session's computed results, rather than fabricated. This
#' is a heuristic safety net, not a proof: it extracts numeric tokens from
#' the narrative and checks what fraction also appear (rounded) among the
#' numeric values already recorded in provenance. A low overlap is surfaced
#' as a guardrail warning, not a hard block, since prose legitimately
#' contains numbers (step counts, dates) unrelated to results.
#'
#' @export
verify_narrative_grounding <- function(narrative, store) {
  if (is.null(narrative) || !nzchar(narrative)) return(list(ok = TRUE, warnings = character(0)))
  narrative_nums <- unique(as.numeric(regmatches(narrative, gregexpr("[0-9]+\\.?[0-9]*", narrative))[[1]]))
  narrative_nums <- narrative_nums[!is.na(narrative_nums)]
  if (length(narrative_nums) == 0) return(list(ok = TRUE, warnings = character(0)))

  computed_text <- paste(vapply(store$entries, function(e) {
    tryCatch(paste(unlist(e$arguments), collapse = " "), error = function(e) "")
  }, character(1)), collapse = " ")
  computed_nums <- unique(as.numeric(regmatches(computed_text, gregexpr("[0-9]+\\.?[0-9]*", computed_text))[[1]]))
  computed_nums <- computed_nums[!is.na(computed_nums)]

  if (length(computed_nums) == 0) {
    return(list(ok = TRUE, warnings = "Narrative grounding could not be verified: no computed numeric results available yet."))
  }
  matched <- sum(sapply(narrative_nums, function(n) any(abs(computed_nums - n) < max(1e-6, abs(n) * 0.01))))
  frac <- matched / length(narrative_nums)
  if (frac < 0.3) {
    return(list(ok = TRUE, warnings = sprintf(
      "LOW confidence narrative grounding: only %.0f%% of numeric claims in the summary matched computed results. Review before trusting the interpretation.",
      100 * frac)))
  }
  list(ok = TRUE, warnings = character(0))
}

#' Strip anything secret-shaped out of a log line before it is written
#' anywhere (console, trace, report).
#' @export
guardrail_scrub_log <- function(line, cfg) {
  redact_secrets(line, cfg)
}
