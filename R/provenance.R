# R/provenance.R
#
# Provenance tracking: every deterministic tool execution and every agent
# decision is recorded in a single append-only, in-session provenance log
# so the UI trace and the final report are built from the same source of
# truth.

#' Create a new empty provenance store (an environment, mutated in place).
#' @export
new_provenance_store <- function() {
  store <- new.env(parent = emptyenv())
  store$entries <- list()
  store$objects <- list()
  class(store) <- "provenance_store"
  store
}

#' Register a computed object (e.g. a Spectra, MsExperiment, QFeatures) under
#' a stable identifier so tool calls can reference inputs/outputs by id
#' rather than passing large Bioconductor objects through the LLM.
#'
#' @export
provenance_put_object <- function(store, id, object) {
  store$objects[[id]] <- object
  invisible(id)
}

#' @export
provenance_get_object <- function(store, id) {
  store$objects[[id]]
}

#' Record one tool-call / agent-decision entry in the provenance log.
#'
#' @param store provenance_store
#' @param agent character agent name (e.g. "data_intake")
#' @param objective character user objective this step served
#' @param plan_step integer step index within the plan
#' @param tool character tool name
#' @param reason character free-text reason the tool was selected
#' @param arguments list of validated arguments (already guardrail-checked)
#' @param r_function character name of the deterministic R function executed
#' @param input_id character or NA, id of the input object
#' @param output_id character or NA, id of the output object
#' @param status one of "ok", "error", "blocked"
#' @param duration_s numeric seconds
#' @param warnings character vector of warning messages
#' @param artifacts character vector of artifact paths/ids produced
#' @export
provenance_add_entry <- function(store, agent, objective, plan_step, tool,
                                  reason, arguments, r_function,
                                  input_id = NA_character_, output_id = NA_character_,
                                  status = "ok", duration_s = NA_real_,
                                  warnings = character(0), artifacts = character(0)) {
  entry <- list(
    timestamp   = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
    agent       = agent,
    objective   = objective,
    plan_step   = plan_step,
    tool        = tool,
    reason      = reason,
    arguments   = arguments,
    r_function  = r_function,
    input_id    = input_id,
    output_id   = output_id,
    status      = status,
    duration_s  = duration_s,
    warnings    = warnings,
    artifacts   = artifacts
  )
  store$entries[[length(store$entries) + 1]] <- entry
  invisible(entry)
}

#' Return the provenance log as a data.frame suitable for the trace table.
#' @export
provenance_as_dataframe <- function(store) {
  if (length(store$entries) == 0) {
    return(data.frame(
      timestamp = character(0), agent = character(0), objective = character(0),
      plan_step = integer(0), tool = character(0), reason = character(0),
      r_function = character(0), input_id = character(0), output_id = character(0),
      status = character(0), duration_s = numeric(0),
      warnings = character(0), artifacts = character(0),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, lapply(store$entries, function(e) {
    data.frame(
      timestamp  = e$timestamp,
      agent      = e$agent,
      objective  = e$objective,
      plan_step  = e$plan_step %||% NA_integer_,
      tool       = e$tool,
      reason     = e$reason,
      r_function = e$r_function,
      input_id   = ifelse(is.null(e$input_id), NA_character_, e$input_id),
      output_id  = ifelse(is.null(e$output_id), NA_character_, e$output_id),
      status     = e$status,
      duration_s = ifelse(is.null(e$duration_s), NA_real_, e$duration_s),
      warnings   = paste(e$warnings, collapse = "; "),
      artifacts  = paste(e$artifacts, collapse = "; "),
      stringsAsFactors = FALSE
    )
  }))
}

#' Serialize the full provenance log (entries only, not R objects) to a list
#' safe for jsonlite::toJSON.
#' @export
provenance_as_list <- function(store) {
  lapply(store$entries, function(e) {
    e$arguments <- e$arguments # already a plain list, kept as-is
    e
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
