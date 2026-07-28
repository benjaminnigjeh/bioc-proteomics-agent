# R/config.R
#
# Central configuration reader. All Claude / runtime configuration comes
# exclusively from environment variables. Nothing here should ever print,
# log, or return the raw API key value to the UI.

#' Read application configuration from environment variables.
#'
#' @return A list of configuration values with safe defaults applied.
#' @export
get_app_config <- function() {
  list(
    anthropic_api_key      = Sys.getenv("ANTHROPIC_API_KEY", unset = ""),
    anthropic_model        = Sys.getenv("ANTHROPIC_MODEL", unset = "claude-sonnet-4-5"),
    llm_mode                = tolower(Sys.getenv("LLM_MODE", unset = "mock")),
    max_agent_steps         = suppressWarnings(as.integer(Sys.getenv("MAX_AGENT_STEPS", unset = "12"))),
    max_tool_retries        = suppressWarnings(as.integer(Sys.getenv("MAX_TOOL_RETRIES", unset = "3"))),
    claude_request_timeout  = suppressWarnings(as.integer(Sys.getenv("CLAUDE_REQUEST_TIMEOUT", unset = "120"))),
    shiny_port               = suppressWarnings(as.integer(Sys.getenv("SHINY_PORT", unset = "3838"))),
    max_upload_mb            = suppressWarnings(as.numeric(Sys.getenv("MAX_UPLOAD_MB", unset = "500")))
  )
}

#' Validate configuration and normalize defaults.
#'
#' Never throws for a missing API key -- the app must run in mock mode or
#' show a graceful "Claude not connected" state instead of crashing.
#'
#' @param cfg Config list from get_app_config().
#' @return Validated config list, with NA/invalid numerics replaced by
#'   documented defaults.
#' @export
validate_app_config <- function(cfg) {
  if (!cfg$llm_mode %in% c("claude", "mock")) {
    warning(sprintf("Unknown LLM_MODE '%s'; falling back to 'mock'.", cfg$llm_mode))
    cfg$llm_mode <- "mock"
  }
  if (is.na(cfg$max_agent_steps) || cfg$max_agent_steps < 1) cfg$max_agent_steps <- 12
  if (cfg$max_agent_steps > 50) cfg$max_agent_steps <- 50
  if (is.na(cfg$max_tool_retries) || cfg$max_tool_retries < 0) cfg$max_tool_retries <- 3
  if (cfg$max_tool_retries > 10) cfg$max_tool_retries <- 10
  if (is.na(cfg$claude_request_timeout) || cfg$claude_request_timeout < 1) cfg$claude_request_timeout <- 120
  if (is.na(cfg$shiny_port)) cfg$shiny_port <- 3838
  if (is.na(cfg$max_upload_mb) || cfg$max_upload_mb <= 0) cfg$max_upload_mb <- 500

  if (cfg$llm_mode == "claude" && !nzchar(cfg$anthropic_api_key)) {
    warning("LLM_MODE=claude but ANTHROPIC_API_KEY is not set. Claude features will be disabled until a key is provided.")
  }
  cfg
}

#' Whether Claude API calls are actually possible right now.
#' @export
claude_is_configured <- function(cfg) {
  cfg$llm_mode == "claude" && nzchar(cfg$anthropic_api_key)
}

#' Redact any secret-shaped values from a character string, for safe logging.
#'
#' @param text Character scalar to redact.
#' @param cfg Config list (used to redact the actual configured key if present).
#' @export
redact_secrets <- function(text, cfg = NULL) {
  if (is.null(text) || !is.character(text)) return(text)
  out <- text
  if (!is.null(cfg) && nzchar(cfg$anthropic_api_key)) {
    out <- gsub(cfg$anthropic_api_key, "[REDACTED]", out, fixed = TRUE)
  }
  # Redact anything that looks like an Anthropic API key regardless of source
  out <- gsub("sk-ant-[A-Za-z0-9\\-_]{10,}", "[REDACTED]", out, perl = TRUE)
  out <- gsub("(?i)(authorization|x-api-key)\\s*[:=]\\s*\\S+", "\\1: [REDACTED]", out, perl = TRUE)
  out
}

#' Locate the application root directory (the directory containing app.R),
#' searching upward from the current working directory. This lets code
#' reference bundled files (like the report template) by a stable path
#' regardless of the working directory a caller happens to be in --
#' notably, testthat::test_dir() changes the working directory to
#' tests/testthat for the duration of a test run.
#'
#' @export
app_root <- function() {
  dir <- normalizePath(getwd(), winslash = "/")
  for (i in 1:6) {
    if (file.exists(file.path(dir, "app.R"))) return(dir)
    parent <- dirname(dir)
    if (identical(parent, dir)) break
    dir <- parent
  }
  normalizePath(getwd(), winslash = "/")
}

#' Bioconductor version string, used for display + report provenance.
#' @export
get_bioc_version <- function() {
  tryCatch({
    if (requireNamespace("BiocManager", quietly = TRUE)) {
      as.character(BiocManager::version())
    } else {
      NA_character_
    }
  }, error = function(e) NA_character_)
}
