# modules/mod_agent.R
#
# Agent Workspace panel: objective input, Create Plan / Run Plan / Stop,
# active agent, tool calls, agent trace, warnings, computed results, and
# the Claude-assisted interpretation.

DEFAULT_OBJECTIVE <- paste(
  "Inspect this LC-MS/MS experiment, evaluate its data quality, apply a conservative MS2 peak filter,",
  "compare the data before and after processing, and prepare a reproducible report."
)

mod_agent_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    bslib::card(
      bslib::card_header("Objective"),
      bslib::card_body(
        shiny::textAreaInput(ns("objective"), "Analysis objective", value = DEFAULT_OBJECTIVE, rows = 5),
        shiny::actionButton(ns("create_plan"), "Create Plan", class = "btn-primary"),
        shiny::actionButton(ns("run_plan"), "Run Plan", class = "btn-success"),
        shiny::actionButton(ns("stop_plan"), "Stop", class = "btn-outline-danger"),
        htmltools::tags$hr(),
        shiny::uiOutput(ns("agent_status"))
      )
    ),
    bslib::card(
      bslib::card_header("Plan"),
      bslib::card_body(shiny::uiOutput(ns("plan_ui")))
    ),
    bslib::card(
      bslib::card_header("Agent Trace"),
      bslib::card_body(
        shiny::tableOutput(ns("trace_table")),
        shiny::downloadButton(ns("download_trace"), "Download Trace CSV")
      )
    ),
    bslib::card(
      bslib::card_header("Warnings"),
      bslib::card_body(shiny::uiOutput(ns("warnings_ui")))
    ),
    bslib::card(
      bslib::card_header("Claude-Assisted Interpretation"),
      bslib::card_body(shiny::uiOutput(ns("narrative_ui")))
    )
  )
}

mod_agent_server <- function(id, shared, ctx) {
  shiny::moduleServer(id, function(input, output, session) {

    run_status <- shiny::reactiveVal("idle")

    shiny::observeEvent(input$create_plan, {
      available <- .build_available_context(shared)
      shiny::withProgress(message = "Creating plan...", {
        plan <- agent_create_plan(input$objective, shared$cfg$llm_mode, shared$cfg, available, AGENT_DESCRIPTIONS)
        shared$plan <- plan
      })
    })

    output$plan_ui <- shiny::renderUI({
      shiny::req(shared$plan)
      if (!is.null(shared$plan$error)) {
        return(htmltools::tags$p(class = "status-badge-warn", paste0(shared$plan$error$type, ": ", shared$plan$error$message)))
      }
      htmltools::tags$pre(class = "plan-text", shared$plan$plan_text)
    })

    shiny::observeEvent(input$stop_plan, {
      shared$stop_requested <- TRUE
    })

    shiny::observeEvent(input$run_plan, {
      shiny::req(shared$plan, is.null(shared$plan$error))
      shared$stop_requested <- FALSE
      run_status("running")

      result <- shiny::withProgress(message = "Running agent plan...", {
        agent_execute_plan(
          plan = shared$plan, objective = input$objective, mode = shared$cfg$llm_mode,
          cfg = shared$cfg, ctx = ctx, agent_descriptions = AGENT_DESCRIPTIONS,
          # Note: this callback deliberately does NOT pump later::run_now()
          # to try to keep the UI "live" during the loop -- doing so
          # re-enters Shiny's own reactive/message-processing machinery
          # from inside an already-running handler and risks corrupting the
          # session. The Stop button and trace table are therefore only
          # checked/updated once the whole plan run completes; this is
          # the "best effort" limitation documented in README/SECURITY.
          on_step = function(entry) {
            shared$trace_bump <- shared$trace_bump + 1L
          },
          stop_check = function() isTRUE(shiny::isolate(shared$stop_requested))
        )
      })

      shared$final_narrative <- result$final_text
      shared$agent_status <- result$status
      run_status(result$status)

      qc_step <- Filter(function(e) identical(e$tool, "calculate_qc_metrics"), result$trace)
      cmp_step <- Filter(function(e) identical(e$tool, "compare_spectra") || identical(e$tool, "filter_spectra"), result$trace)
      if (length(qc_step) > 0 && is.null(shared$qc_metrics)) {
        sp <- if (!is.null(shared$spectra_id)) provenance_get_object(shared$store, shared$spectra_id) else NULL
        if (!is.null(sp)) shared$qc_metrics <- calculate_qc_metrics(sp)
      }
      if (length(result$results[["filter_spectra"]]) > 0) {
        shared$processing_comparison <- result$results[["filter_spectra"]]$comparison
        shared$processing_stage_id <- result$results[["filter_spectra"]]$stage_id
      }
    })

    output$agent_status <- shiny::renderUI({
      st <- run_status()
      cls <- switch(st, complete = "status-badge-ok", error = "status-badge-warn",
                    max_steps_reached = "status-badge-warn", stopped = "text-warning", "text-muted")
      htmltools::tags$p(class = cls, sprintf("Status: %s | LLM mode: %s", st, shared$cfg$llm_mode))
    })

    output$trace_table <- shiny::renderTable({
      shared$trace_bump
      df <- provenance_as_dataframe(shared$store)
      shiny::req(nrow(df) > 0)
      utils::tail(df[, c("timestamp", "agent", "tool", "status", "duration_s", "output_id", "warnings")], 100)
    })

    output$download_trace <- shiny::downloadHandler(
      filename = function() "agent_trace.csv",
      content = function(file) utils::write.csv(provenance_as_dataframe(shared$store), file, row.names = FALSE)
    )

    output$warnings_ui <- shiny::renderUI({
      shared$trace_bump
      df <- provenance_as_dataframe(shared$store)
      w <- unique(df$warnings[nzchar(df$warnings)])
      if (length(w) == 0) return(htmltools::tags$p(class = "text-muted", "No warnings."))
      htmltools::tags$ul(lapply(w, htmltools::tags$li))
    })

    output$narrative_ui <- shiny::renderUI({
      shiny::req(shared$final_narrative)
      label <- if (shared$cfg$llm_mode == "claude") "AI-assisted interpretation generated using Claude" else "[MOCK LLM] Deterministic interpretation (no external API call)"
      htmltools::tagList(
        htmltools::tags$p(htmltools::tags$strong(label)),
        htmltools::tags$p(shared$final_narrative)
      )
    })
  })
}

.build_available_context <- function(shared) {
  list(
    spectra_id = shared$spectra_id,
    source_filename = shared$source_filename,
    has_psm = !is.null(shared$psm_id), psm_id = shared$psm_id,
    has_quant = !is.null(shared$quant_table_id), quant_table_id = shared$quant_table_id,
    quant_id_col = shared$quant_id_col, quant_sample_cols = shared$quant_sample_cols
  )
}
