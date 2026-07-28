# R/mock_llm.R
#
# Deterministic mock LLM backend. Implements the same "plan then bounded
# tool-use loop" interface as R/claude_client.R so the rest of the app
# (agent_execution.R, agent_supervisor.R) never needs to know which mode
# is active. Used for LLM_MODE=mock, all automated tests, and CI -- it
# never makes a network request.

#' Build a deterministic multi-step plan from the user's objective and
#' what is currently available in the session (spectra, PSM table, quant
#' table). Every step stores an `args_fn(state)` closure so argument
#' values that only exist after a prior step runs (e.g. a freshly created
#' object id) can be resolved lazily, exactly like a real agent would use
#' the previous tool's result.
#'
#' @export
mock_generate_plan <- function(objective, available, cfg) {
  steps <- list()
  n <- 0L
  add_step <- function(agent, tool, reason, args_fn) {
    n <<- n + 1L
    steps[[n]] <<- list(step = n, agent = agent, tool = tool, reason = reason, args_fn = args_fn)
  }

  if (!is.null(available$spectra_id)) {
    add_step("data_intake", "summarize_experiment",
              "Establish a baseline understanding of the imported experiment before further analysis.",
              function(state) list(spectra_id = available$spectra_id))

    add_step("qc", "calculate_qc_metrics",
              "The objective requires evaluating data quality before any processing decisions are made.",
              function(state) list(spectra_id = available$spectra_id))

    add_step("spectrum_processing", "filter_spectra",
              "Apply a conservative MS2 relative-intensity filter as requested, preserving the original stage.",
              function(state) list(spectra_id = available$spectra_id, stage_name = "ms2_filtered",
                                    ms_level = list(2L), min_relative_intensity = 0.01))

    add_step("spectrum_processing", "compare_spectra",
              "Quantify the before/after effect of the applied filter to support the report.",
              function(state) list(spectra_id_before = available$spectra_id,
                                    spectra_id_after = state$outputs[["filter_spectra"]]))
  }

  if (isTRUE(available$has_psm) && !is.null(available$psm_id)) {
    add_step("identification", "summarize_identifications",
              "A PSM identification table is already available; summarize PSM/peptide/protein counts for the report.",
              function(state) list(psm_id = available$psm_id))
  }

  if (isTRUE(available$has_quant) && !is.null(available$quant_table_id) &&
      !is.null(available$quant_id_col) && length(available$quant_sample_cols) > 0) {
    add_step("quantification", "build_qfeatures",
              "A quantitative abundance table is already available; construct a QFeatures object as the canonical representation.",
              function(state) list(table_id = available$quant_table_id,
                                    id_col = available$quant_id_col, sample_cols = available$quant_sample_cols,
                                    assay_name = "peptides"))
  }

  add_step("reporting", "generate_report",
            "The objective requires a reproducible report summarizing all computed results.",
            function(state) list(objective = objective))

  plan_text <- paste0(
    "Plan for objective: \"", objective, "\"\n",
    paste(vapply(steps, function(s) sprintf("%d. [%s] %s -- %s", s$step, s$agent, s$tool, s$reason), character(1)),
          collapse = "\n")
  )

  list(plan_text = plan_text, steps = steps, source = "mock")
}

#' Advance the mock loop by one action. Mirrors the shape returned by
#' claude_next_action() in claude_client.R:
#'   list(type = "tool_use", agent, tool, reason, args, plan_step)
#'   list(type = "final", text)
#'
#' @export
mock_next_action <- function(state) {
  if (state$cursor > length(state$plan$steps)) {
    return(list(type = "final", text = mock_final_summary(state)))
  }
  step <- state$plan$steps[[state$cursor]]
  args <- step$args_fn(state)
  list(type = "tool_use", agent = step$agent, tool = step$tool, reason = step$reason,
       args = args, plan_step = step$step)
}

#' Deterministic, template-based "interpretation" grounded in the numeric
#' results collected during the run. Prefixed distinctly from real Claude
#' output so the UI/report can never confuse the two.
#' @export
mock_final_summary <- function(state) {
  qc <- state$results[["calculate_qc_metrics"]]
  cmp <- state$results[["compare_spectra"]]
  parts <- c("[MOCK LLM] Deterministic run summary (no external API call was made).")
  if (!is.null(qc)) {
    parts <- c(parts, sprintf(
      "The experiment contains %s spectra (%s MS1, %s MS2; MS2/MS1 ratio %.2f).",
      qc$n_spectra, qc$n_ms1, qc$n_ms2, qc$ms2_ms1_ratio %||% NA_real_))
  }
  if (!is.null(cmp)) {
    delta <- cmp$comparison$delta %||% cmp$delta
    if (!is.null(delta)) {
      parts <- c(parts, sprintf(
        "After the conservative MS2 filter, total peak count changed by %.1f%% relative to the input stage.",
        delta$total_peaks_pct_change %||% NA_real_))
    }
  }
  parts <- c(parts, "A reproducible HTML report has been generated with full provenance.")
  paste(parts, collapse = " ")
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
