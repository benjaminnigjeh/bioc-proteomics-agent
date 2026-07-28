# R/agent_supervisor.R
#
# Supervisor-level glue: agent descriptions shown in the UI and sent to
# Claude, the per-session execution context (`ctx`) that tools execute
# against, and small helpers for describing Claude connection status.

AGENT_DESCRIPTIONS <- list(
  data_intake = "Validates uploads, imports mzML/mzXML/MGF into Spectra/MsExperiment objects, and reports file metadata and checksums.",
  qc = "Calculates file- and spectrum-level QC metrics, flags data-quality concerns with severity, and produces QC plots.",
  spectrum_processing = "Applies filters, normalization, and top-N peak retention as new named processing stages, and compares before/after metrics.",
  identification = "Imports and validates PSM tables, filters by score/FDR, and summarizes peptide/protein identifications.",
  quantification = "Builds QFeatures objects from abundance tables, normalizes and aggregates them, and runs PCA / two-group comparisons.",
  reporting = "Combines all computed results, warnings, and provenance into a reproducible HTML report."
)

#' Build a new per-session execution context. `store` is a provenance_store
#' (see R/provenance.R). `uploads` is a named-list registry of validated
#' uploads keyed by file_id: list(path, original_filename, sha256).
#'
#' @export
new_session_ctx <- function(store, session_dir, uploads_env, report_params_builder) {
  list(
    store = store,
    session_dir = session_dir,
    get_upload = function(file_id) {
      if (!exists(file_id, envir = uploads_env, inherits = FALSE)) return(NULL)
      get(file_id, envir = uploads_env, inherits = FALSE)
    },
    build_report_params = report_params_builder
  )
}

#' Register a validated upload into the session's upload registry, and
#' return its file_id.
#' @export
register_upload <- function(uploads_env, path, original_filename) {
  file_id <- paste0("file_", format(Sys.time(), "%H%M%OS3"), "_", paste(sample(letters, 4), collapse = ""))
  assign(file_id, list(path = path, original_filename = original_filename,
                        sha256 = file_checksum(path)), envir = uploads_env)
  file_id
}

#' Human-readable Claude connection status for the Home panel. Never
#' returns the key itself -- only whether one is configured, and (when
#' `probe = TRUE`) a live one-token connectivity check.
#'
#' @export
claude_connection_status <- function(cfg, probe = FALSE) {
  if (cfg$llm_mode != "claude") {
    return(list(state = "mock_mode", label = "Mock mode active", detail = "LLM_MODE=mock; no Claude API calls will be made.", ok = TRUE))
  }
  if (!nzchar(cfg$anthropic_api_key)) {
    return(list(state = "not_configured", label = "Claude not connected",
                detail = "ANTHROPIC_API_KEY is not set. Add it to .env and restart.", ok = FALSE))
  }
  if (!probe) {
    return(list(state = "configured", label = "Claude configured",
                detail = sprintf("Model: %s (connectivity not yet verified).", cfg$anthropic_model), ok = TRUE))
  }
  res <- claude_messages_call(
    messages = list(list(role = "user", content = "Reply with the single word: ok")),
    tools = NULL, system = NULL, cfg = cfg
  )
  if (!res$ok) {
    return(list(state = "error", label = "Claude connection failed",
                detail = paste0(res$error$type, ": ", res$error$message), ok = FALSE))
  }
  list(state = "connected", label = "Claude connected",
       detail = sprintf("Model: %s", cfg$anthropic_model), ok = TRUE)
}
