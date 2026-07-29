# modules/mod_qc.R
#
# Quality Control panel: MS1/MS2 counts, ratio, RT range, peaks-per-spectrum
# distribution, TIC/BPI summaries, precursor m/z distribution, charge
# distribution, missing precursor metadata, empty spectrum count, warnings.

mod_qc_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(12),
    bslib::card(
      bslib::card_header("Run Quality Control"),
      bslib::card_body(
        shiny::actionButton(ns("run_qc"), "Run QC Agent", class = "btn-primary"),
        shiny::uiOutput(ns("qc_summary"))
      )
    ),
    bslib::card(
      bslib::card_header("QC Warnings"),
      bslib::card_body(shiny::uiOutput(ns("qc_warnings")))
    ),
    bslib::card(
      bslib::card_header("Standardized QC Metrics (MsQuality)"),
      bslib::card_body(
        shiny::actionButton(ns("run_msquality"), "Run MsQuality Metrics", class = "btn-outline-secondary"),
        shiny::uiOutput(ns("msquality_ui"))
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header("TIC (MS1)"), shiny::plotOutput(ns("tic_plot"))),
      bslib::card(bslib::card_header("BPC (MS1)"), shiny::plotOutput(ns("bpc_plot")))
    )
  )
}

mod_qc_server <- function(id, shared, ctx) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observeEvent(input$run_qc, {
      shiny::req(shared$spectra_id)
      sp <- provenance_get_object(shared$store, shared$spectra_id)
      shiny::withProgress(
        message = "Calculating QC metrics...",
        detail = sprintf("Analyzing %s spectra -- larger real datasets take a few seconds.", length(sp)),
        {
        t0 <- Sys.time()
        qc <- calculate_qc_metrics(sp)
        shared$qc_metrics <- qc
        provenance_add_entry(shared$store, agent = "qc", objective = "Manual QC run",
          plan_step = NA_integer_, tool = "calculate_qc_metrics", reason = "User clicked Run QC Agent.",
          arguments = list(spectra_id = shared$spectra_id), r_function = "calculate_qc_metrics",
          input_id = shared$spectra_id, status = "ok",
          duration_s = as.numeric(difftime(Sys.time(), t0, units = "secs")), warnings = qc$warnings)
      })
    })

    output$qc_summary <- shiny::renderUI({
      shiny::req(shared$qc_metrics)
      qc <- shared$qc_metrics
      rows <- list(
        c("Total spectra", qc$n_spectra), c("MS1 count", qc$n_ms1), c("MS2 count", qc$n_ms2),
        c("MS2/MS1 ratio", sprintf("%.3f", qc$ms2_ms1_ratio %||% NA)),
        c("RT range (s)", paste(round(qc$rt_range_s, 1), collapse = " - ")),
        c("Peaks/spectrum (median)", qc$peaks_per_spectrum$median),
        c("TIC (median)", format(qc$tic_summary$median, scientific = TRUE, digits = 3)),
        c("BPI (median)", format(qc$bpi_summary$median, scientific = TRUE, digits = 3)),
        c("Precursor m/z (median)", round(qc$precursor_mz_summary$median %||% NA, 2)),
        c("Charge distribution", paste(names(qc$charge_counts), unlist(qc$charge_counts), sep = "=", collapse = ", ")),
        c("Missing precursor metadata", qc$n_missing_precursor),
        c("Empty spectra", qc$n_empty_spectra)
      )
      htmltools::tags$table(class = "table table-sm",
        htmltools::tags$tbody(lapply(rows, function(r) htmltools::tags$tr(
          htmltools::tags$td(htmltools::tags$strong(r[1])), htmltools::tags$td(as.character(r[2]))))))
    })

    shiny::observeEvent(input$run_msquality, {
      shiny::req(shared$spectra_id)
      sp <- provenance_get_object(shared$store, shared$spectra_id)
      shiny::withProgress(message = "Calculating standardized QC metrics (MsQuality)...", {
        t0 <- Sys.time()
        m <- calculate_msquality_metrics(sp)
        shared$msquality_metrics <- m
        provenance_add_entry(shared$store, agent = "qc", objective = "Manual MsQuality run",
          plan_step = NA_integer_, tool = "calculate_standardized_qc_metrics", reason = "User clicked Run MsQuality Metrics.",
          arguments = list(spectra_id = shared$spectra_id), r_function = "calculate_msquality_metrics",
          input_id = shared$spectra_id, status = "ok",
          duration_s = as.numeric(difftime(Sys.time(), t0, units = "secs")))
      })
    })

    output$msquality_ui <- shiny::renderUI({
      shiny::req(shared$msquality_metrics)
      m <- shared$msquality_metrics
      rows <- lapply(names(m), function(nm) {
        v <- m[[nm]]
        htmltools::tags$tr(htmltools::tags$td(htmltools::tags$strong(nm)),
                            htmltools::tags$td(if (is.numeric(v)) format(v, digits = 4) else as.character(v)))
      })
      htmltools::tags$table(class = "table table-sm", htmltools::tags$tbody(rows))
    })

    output$qc_warnings <- shiny::renderUI({
      shiny::req(shared$qc_metrics)
      w <- shared$qc_metrics$warnings
      if (length(w) == 0) return(htmltools::tags$p(class = "status-badge-ok", "No QC warnings."))
      htmltools::tags$ul(lapply(w, function(x) {
        sev <- sub(":.*$", "", x)
        cls <- if (sev == "HIGH") "status-badge-warn" else "text-warning"
        htmltools::tags$li(class = cls, x)
      }))
    })

    output$tic_plot <- shiny::renderPlot({
      shiny::req(shared$spectra_id)
      sp <- provenance_get_object(shared$store, shared$spectra_id)
      plot_tic(sp, ms_level = 1L)
    })

    output$bpc_plot <- shiny::renderPlot({
      shiny::req(shared$spectra_id)
      sp <- provenance_get_object(shared$store, shared$spectra_id)
      plot_bpc(sp, ms_level = 1L)
    })
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
