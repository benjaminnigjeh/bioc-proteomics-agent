# R/agent_execution.R
#
# The bounded multi-agent tool-use loop. Works identically for
# LLM_MODE=mock and LLM_MODE=claude: this file never knows which backend
# is active except through the `mode` string, and always routes tool
# execution through R/agent_guardrails.R. This is what prevents
# recursive/endless tool use and enforces MAX_AGENT_STEPS.

#' Ask the active backend for a reviewable plan (no tools are executed).
#'
#' @export
agent_create_plan <- function(objective, mode, cfg, available, agent_descriptions) {
  if (mode == "mock") {
    return(mock_generate_plan(objective, available, cfg))
  }
  res <- claude_generate_plan(objective, available, agent_descriptions, cfg)
  if (!res$ok) {
    return(list(plan_text = NULL, steps = list(), source = "claude", error = res$error))
  }
  # Claude's plan is narrative; the actual tool sequence is decided live
  # during execution via the bounded tool-use loop, so `steps` starts empty.
  list(plan_text = res$plan_text, steps = list(), source = "claude")
}

#' Initialize the execution state shared by both backends.
#'
#' `available` (the same session-context list passed to `agent_create_plan`)
#' is embedded in the first user message so the execution loop actually
#' knows the IDs (spectra_id, psm_id, ...) it can call tools against --
#' `agent_create_plan` sees `available` for planning, but without this the
#' execution loop would only ever see the free-text objective and have no
#' grounded way to reference already-loaded data.
.new_agent_state <- function(objective, plan, system_prompt, available = NULL) {
  context_block <- if (!is.null(available)) {
    paste0("\n\nSession context (already available, do not re-derive; use these IDs directly in tool arguments):\n",
           jsonlite::toJSON(available, auto_unbox = TRUE, null = "null"))
  } else {
    ""
  }
  list(
    objective = objective,
    plan = plan,
    cursor = 1L,
    outputs = list(),
    results = list(),
    messages = list(list(role = "user", content = sprintf(
      "Objective: %s%s\n\nExecute this objective step by step using only the registered tools. After each tool result, decide the next justified tool call, or provide a final grounded summary when the objective is satisfied.",
      objective, context_block
    ))),
    system_prompt = system_prompt,
    trace = list(),
    warnings = character(0),
    final_text = NULL,
    status = "running"
  )
}

#' Run the bounded agent execution loop.
#'
#' @param plan result of agent_create_plan()
#' @param objective character
#' @param mode "mock" or "claude"
#' @param cfg validated app config
#' @param ctx session execution context (see agent_supervisor.R:new_session_ctx)
#' @param agent_descriptions named list, agent name -> description
#' @param available optional session-context list (same shape passed to
#'   agent_create_plan) so the execution loop knows the actual IDs
#'   (spectra_id, psm_id, ...) it can call tools against
#' @param on_step optional function(trace_entry) called after each step for
#'   live UI updates
#' @param stop_check optional function() -> logical, checked each iteration
#'   to support the "Stop" button
#' @export
agent_execute_plan <- function(plan, objective, mode, cfg, ctx, agent_descriptions,
                                available = NULL, on_step = NULL, stop_check = NULL) {
  if (!is.null(plan$error)) {
    return(list(status = "error", error = plan$error, trace = list(), final_text = NULL))
  }

  tools_meta <- list_tools_for_claude()
  system_prompt <- .build_system_prompt(agent_descriptions)
  state <- .new_agent_state(objective, plan, system_prompt, available)

  max_steps <- cfg$max_agent_steps
  step_i <- 0L

  repeat {
    if (!is.null(stop_check) && isTRUE(stop_check())) {
      state$status <- "stopped"
      break
    }
    step_i <- step_i + 1L
    if (step_i > max_steps) {
      state$status <- "max_steps_reached"
      state$warnings <- c(state$warnings, sprintf("Stopped after reaching the maximum of %d agent steps.", max_steps))
      break
    }

    action <- if (mode == "mock") mock_next_action(state) else claude_next_action(state, tools_meta, cfg)

    if (isFALSE(action$ok) || (!is.null(action$error))) {
      state$status <- "error"
      state$warnings <- c(state$warnings, sprintf("Non-recoverable agent error: %s", action$error$message %||% "unknown error"))
      entry <- provenance_add_entry(ctx$store, agent = "supervisor", objective = objective,
        plan_step = step_i, tool = NA_character_, reason = "LLM backend error",
        arguments = list(), r_function = NA_character_, status = "error",
        warnings = action$error$message %||% "unknown error")
      state$trace[[length(state$trace) + 1]] <- entry
      if (!is.null(on_step)) on_step(entry)
      break
    }

    if (identical(action$type, "final")) {
      grounding <- verify_narrative_grounding(action$text, ctx$store, state$results)
      state$final_text <- action$text
      state$warnings <- c(state$warnings, grounding$warnings)
      state$status <- "complete"
      break
    }

    # type == "tool_use"
    validated <- guardrail_validate_call(action$tool, action$args, ctx)
    if (!validated$ok) {
      entry <- provenance_add_entry(ctx$store, agent = action$agent %||% "supervisor", objective = objective,
        plan_step = action$plan_step %||% step_i, tool = action$tool %||% "unknown",
        reason = action$reason %||% "", arguments = action$args %||% list(),
        r_function = NA_character_, status = "blocked", warnings = validated$errors)
      state$trace[[length(state$trace) + 1]] <- entry
      if (!is.null(on_step)) on_step(entry)

      if (mode == "claude") {
        # Give Claude a chance to correct malformed/invalid arguments within
        # the remaining step budget, rather than aborting the whole run.
        state$messages[[length(state$messages) + 1]] <- list(role = "assistant", content = action$assistant_content)
        state$messages[[length(state$messages) + 1]] <- list(role = "user", content = list(list(
          type = "tool_result", tool_use_id = action$tool_use_id,
          content = paste("Guardrail rejected this tool call:", validated$errors), is_error = TRUE
        )))
        next
      } else {
        state$status <- "error"
        state$warnings <- c(state$warnings, paste("Guardrail blocked planned step:", validated$errors))
        break
      }
    }

    exec_res <- guardrail_execute_call(validated$tool, validated$args, ctx)
    status <- if (exec_res$ok) "ok" else "error"

    entry <- provenance_add_entry(ctx$store, agent = action$agent %||% .agent_for_tool(action$tool),
      objective = objective, plan_step = action$plan_step %||% step_i, tool = action$tool,
      reason = action$reason %||% "", arguments = validated$args, r_function = action$tool,
      input_id = .first_id_arg(validated$args), output_id = exec_res$output_id %||% NA_character_,
      status = status, duration_s = exec_res$duration_s,
      warnings = if (exec_res$ok) exec_res$warnings else exec_res$error,
      artifacts = if (exec_res$ok) exec_res$artifacts else character(0))
    state$trace[[length(state$trace) + 1]] <- entry
    if (!is.null(on_step)) on_step(entry)

    # Record outputs/results for BOTH backends -- not just mock -- so the
    # UI can sync tab state (identification/quantification/etc.) from a
    # real Claude-driven run exactly the same way it does from mock runs.
    if (exec_res$ok) {
      state$outputs[[action$tool]] <- exec_res$output_id
      state$results[[action$tool]] <- exec_res$output
    }

    if (mode == "mock") {
      if (exec_res$ok) {
        state$cursor <- state$cursor + 1L
      } else {
        state$status <- "error"
        state$warnings <- c(state$warnings, paste("Tool execution failed:", exec_res$error))
        break
      }
    } else {
      state$messages[[length(state$messages) + 1]] <- list(role = "assistant", content = action$assistant_content)
      tool_result_text <- if (exec_res$ok) {
        jsonlite::toJSON(exec_res$output, auto_unbox = TRUE, null = "null")
      } else {
        paste("Tool execution error:", exec_res$error)
      }
      state$messages[[length(state$messages) + 1]] <- list(role = "user", content = list(list(
        type = "tool_result", tool_use_id = action$tool_use_id,
        content = as.character(tool_result_text), is_error = !exec_res$ok
      )))
    }
  }

  list(status = state$status, trace = state$trace, final_text = state$final_text,
       warnings = state$warnings, results = state$results)
}

.build_system_prompt <- function(agent_descriptions) {
  paste0(
    "You are the Supervisor Agent coordinating a team of specialist Bioconductor proteomics agents. ",
    "You never compute scientific results yourself and never write or execute code -- you only call ",
    "the registered tools provided to you, one at a time, and reason about their JSON results. ",
    "Specialist agents:\n",
    paste(sprintf("- %s: %s", names(agent_descriptions), unlist(agent_descriptions)), collapse = "\n"),
    "\n\nWhen the objective is fully satisfied, respond with plain text only (no tool call) summarizing ",
    "findings, citing only numbers that appeared in tool results. Always generate a report via the ",
    "generate_report tool before concluding if the objective mentions a report."
  )
}

.agent_for_tool <- function(tool_name) {
  m <- list(
    inspect_uploaded_file = "data_intake", import_ms_file = "data_intake", summarize_experiment = "data_intake",
    list_spectra = "data_intake", get_spectrum = "data_intake",
    calculate_qc_metrics = "qc", plot_tic = "qc", plot_bpc = "qc", plot_spectrum = "qc",
    filter_spectra = "spectrum_processing", normalize_spectra = "spectrum_processing",
    retain_top_peaks = "spectrum_processing", compare_spectra = "spectrum_processing",
    import_psm_table = "identification", filter_psms = "identification", summarize_identifications = "identification",
    import_quant_table = "quantification", build_qfeatures = "quantification",
    normalize_quantification = "quantification", aggregate_to_proteins = "quantification",
    run_exploratory_analysis = "quantification",
    generate_report = "reporting"
  )
  m[[tool_name]] %||% "supervisor"
}

.first_id_arg <- function(args) {
  id_fields <- c("spectra_id", "spectra_id_before", "psm_id", "table_id", "qfeatures_id", "file_id")
  for (f in id_fields) if (!is.null(args[[f]])) return(as.character(args[[f]]))
  NA_character_
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
