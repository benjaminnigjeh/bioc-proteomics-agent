# modules/mod_agent.R
#
# Agent Workspace panel: objective input, Create Plan / Run Plan / Stop,
# active agent, tool calls, agent trace, warnings, computed results, and
# the Claude-assisted interpretation.

DEFAULT_OBJECTIVE <- paste(
  "Inspect this LC-MS/MS experiment, evaluate its data quality, apply a conservative MS2 peak filter,",
  "compare the data before and after processing, and prepare a reproducible report."
)

# Single, full-width vertical flow rather than a cramped side-by-side
# grid: each stage of the loop (objective -> plan -> run -> trace ->
# warnings -> interpretation) gets its own generously-sized, full-width
# section, and the page scrolls naturally instead of packing everything
# into fixed-height boxes with their own inner scrollbars.
mod_agent_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    bslib::card(
      class = "mb-3",
      bslib::card_header(htmltools::tags$span(htmltools::tags$i(class = "fa-solid fa-bullseye me-2"), "1. Objective")),
      bslib::card_body(
        shiny::textAreaInput(ns("objective"), "Analysis objective", value = DEFAULT_OBJECTIVE, rows = 4, width = "100%"),
        htmltools::tags$div(
          class = "d-flex gap-2 flex-wrap",
          shiny::actionButton(ns("create_plan"), htmltools::tagList(htmltools::tags$i(class = "fa-solid fa-diagram-project"), " Create Plan"), class = "btn-primary"),
          shiny::actionButton(ns("run_plan"), htmltools::tagList(htmltools::tags$i(class = "fa-solid fa-play"), " Run Plan"), class = "btn-success"),
          shiny::actionButton(ns("stop_plan"), htmltools::tagList(htmltools::tags$i(class = "fa-solid fa-stop"), " Stop"), class = "btn-outline-danger")
        ),
        htmltools::tags$hr(),
        shiny::uiOutput(ns("agent_status"))
      )
    ),
    bslib::card(
      class = "mb-3",
      bslib::card_header(htmltools::tags$span(htmltools::tags$i(class = "fa-solid fa-map me-2"), "2. Plan")),
      bslib::card_body(shiny::uiOutput(ns("plan_ui")))
    ),
    bslib::card(
      class = "mb-3",
      bslib::card_header(htmltools::tags$span(htmltools::tags$i(class = "fa-solid fa-list-check me-2"), "3. Agent Trace")),
      bslib::card_body(
        shiny::downloadButton(ns("download_trace"), htmltools::tagList(htmltools::tags$i(class = "fa-solid fa-download"), " Download Trace CSV"), class = "btn-outline-secondary btn-sm mb-3"),
        htmltools::tags$div(class = "table-responsive", shiny::tableOutput(ns("trace_table")))
      )
    ),
    bslib::card(
      class = "mb-3",
      bslib::card_header(htmltools::tags$span(htmltools::tags$i(class = "fa-solid fa-triangle-exclamation me-2"), "4. Warnings")),
      bslib::card_body(shiny::uiOutput(ns("warnings_ui")))
    ),
    bslib::card(
      class = "mb-3",
      bslib::card_header(htmltools::tags$span(htmltools::tags$i(class = "fa-solid fa-comment-dots me-2"), "5. Claude-Assisted Interpretation")),
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
          available = .build_available_context(shared),
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
      if (length(qc_step) > 0 && is.null(shared$qc_metrics)) {
        sp <- if (!is.null(shared$spectra_id)) provenance_get_object(shared$store, shared$spectra_id) else NULL
        if (!is.null(sp)) shared$qc_metrics <- calculate_qc_metrics(sp)
      }
      if (!is.null(result$results[["calculate_standardized_qc_metrics"]])) {
        shared$msquality_metrics <- result$results[["calculate_standardized_qc_metrics"]]
      }
      if (length(result$results[["filter_spectra"]]) > 0) {
        shared$processing_comparison <- result$results[["filter_spectra"]]$comparison
        shared$processing_stage_id <- result$results[["filter_spectra"]]$stage_id
      }

      # Propagate identification/quantification results so the
      # Identifications and Quantification tabs (which display shared
      # session state, not agent internals) reflect what the agent just
      # did -- this applies to both mock and Claude-driven runs.
      if (!is.null(result$results[["import_psm_table"]]$psm_id)) {
        shared$psm_id <- result$results[["import_psm_table"]]$psm_id
      }
      if (!is.null(result$results[["filter_psms"]]$stage_id)) {
        shared$psm_id <- result$results[["filter_psms"]]$stage_id
      }
      if (!is.null(result$results[["summarize_identifications"]])) {
        si <- result$results[["summarize_identifications"]]
        shared$ident_summary <- si[intersect(names(si), c("n_psm", "n_unique_peptides", "n_unique_proteins", "median_psms_per_protein"))]
      }
      if (!is.null(result$results[["import_quant_table"]]$table_id)) {
        iq <- result$results[["import_quant_table"]]
        shared$quant_table_id <- iq$table_id
        shared$quant_id_col <- iq$id_col
        shared$quant_sample_cols <- unlist(iq$sample_cols)
      }
      if (!is.null(result$results[["build_qfeatures"]]$qfeatures_id)) {
        bq <- result$results[["build_qfeatures"]]
        shared$qfeatures_id <- bq$qfeatures_id
        shared$quant_assay_name <- bq$assay_name
      }
      if (!is.null(result$results[["normalize_quantification"]]$qfeatures_id)) {
        nq <- result$results[["normalize_quantification"]]
        shared$qfeatures_id <- nq$qfeatures_id
        shared$quant_assay_name <- nq$new_assay
      }
      if (!is.null(result$results[["aggregate_to_proteins"]]$qfeatures_id)) {
        ap <- result$results[["aggregate_to_proteins"]]
        shared$qfeatures_id <- ap$qfeatures_id
        shared$quant_assay_name <- ap$new_assay
      }
      if (!is.null(result$results[["run_exploratory_analysis"]])) {
        re <- result$results[["run_exploratory_analysis"]]
        shared$quant_summary <- list(
          assay = shared$quant_assay_name, pca_ok = isTRUE(re$pca$ok),
          comparison_ok = isTRUE(re$comparison$ok %||% FALSE)
        )
      }
      if (!is.null(result$results[["import_sample_metadata"]]$metadata_id)) {
        shared$quant_metadata_id <- result$results[["import_sample_metadata"]]$metadata_id
      }
      if (!is.null(result$results[["run_msstats_comparison"]])) {
        rm_ <- result$results[["run_msstats_comparison"]]
        shared$msstats_summary <- list(
          method = rm_$method, n_proteins = rm_$n_proteins, n_significant_padj05 = rm_$n_significant_padj05
        )
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
    quant_id_col = shared$quant_id_col, quant_sample_cols = shared$quant_sample_cols,
    has_sample_metadata = !is.null(shared$quant_metadata_id), quant_metadata_id = shared$quant_metadata_id,
    qfeatures_id = shared$qfeatures_id, quant_assay_name = shared$quant_assay_name
  )
}
