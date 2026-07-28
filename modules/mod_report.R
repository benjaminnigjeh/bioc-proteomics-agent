# modules/mod_report.R
#
# Report panel: report settings, Generate Report, and downloads for the
# HTML report, QC table, processed results, provenance JSON, and trace.

mod_report_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    bslib::card(
      bslib::card_header("Report Settings"),
      bslib::card_body(
        shiny::textAreaInput(ns("objective"), "Objective to include in report", value = DEFAULT_OBJECTIVE, rows = 4),
        shiny::actionButton(ns("generate"), "Generate Report", class = "btn-primary"),
        shiny::uiOutput(ns("status"))
      )
    ),
    bslib::card(
      bslib::card_header("Downloads"),
      bslib::card_body(
        shiny::downloadButton(ns("dl_report"), "Download HTML Report"),
        htmltools::tags$br(), htmltools::tags$br(),
        shiny::downloadButton(ns("dl_qc"), "Download QC Table (CSV)"),
        htmltools::tags$br(), htmltools::tags$br(),
        shiny::downloadButton(ns("dl_processed"), "Download Processed Results (CSV)"),
        htmltools::tags$br(), htmltools::tags$br(),
        shiny::downloadButton(ns("dl_provenance"), "Download Provenance (JSON)"),
        htmltools::tags$br(), htmltools::tags$br(),
        shiny::downloadButton(ns("dl_trace"), "Download Agent Trace (CSV)")
      )
    )
  )
}

mod_report_server <- function(id, shared, ctx) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observeEvent(input$generate, {
      shiny::withProgress(message = "Rendering report...", {
        result <- tryCatch({
          t0 <- Sys.time()
          params <- ctx$build_report_params(input$objective)
          path <- generate_report(params, shared$session_dir)
          shared$report_path <- path
          provenance_add_entry(shared$store, agent = "reporting", objective = input$objective,
            plan_step = NA_integer_, tool = "generate_report", reason = "User clicked Generate Report.",
            arguments = list(objective = input$objective), r_function = "generate_report",
            output_id = NA_character_, status = "ok",
            duration_s = as.numeric(difftime(Sys.time(), t0, units = "secs")), artifacts = path)
          list(ok = TRUE, path = path)
        }, error = function(e) list(ok = FALSE, message = conditionMessage(e)))
      })

      output$status <- shiny::renderUI({
        if (isTRUE(result$ok)) htmltools::tags$p(class = "status-badge-ok", "Report generated successfully.")
        else htmltools::tags$p(class = "status-badge-warn", paste("Report generation failed:", result$message))
      })
    })

    output$dl_report <- shiny::downloadHandler(
      filename = function() "bioc_proteomics_report.html",
      content = function(file) {
        shiny::req(shared$report_path)
        file.copy(shared$report_path, file, overwrite = TRUE)
      }
    )
    output$dl_qc <- shiny::downloadHandler(
      filename = function() "qc_metrics.csv",
      content = function(file) {
        shiny::req(shared$qc_metrics)
        path <- export_qc_table(shared$qc_metrics, shared$session_dir)
        file.copy(path, file, overwrite = TRUE)
      }
    )
    output$dl_processed <- shiny::downloadHandler(
      filename = function() "processed_results.csv",
      content = function(file) {
        shiny::req(shared$processing_comparison)
        path <- export_processed_table(shared$processing_comparison, shared$session_dir)
        file.copy(path, file, overwrite = TRUE)
      }
    )
    output$dl_provenance <- shiny::downloadHandler(
      filename = function() "provenance.json",
      content = function(file) {
        path <- export_provenance_json(shared$store, shared$session_dir)
        file.copy(path, file, overwrite = TRUE)
      }
    )
    output$dl_trace <- shiny::downloadHandler(
      filename = function() "agent_trace.csv",
      content = function(file) {
        path <- export_trace_csv(shared$store, shared$session_dir)
        file.copy(path, file, overwrite = TRUE)
      }
    )
  })
}
