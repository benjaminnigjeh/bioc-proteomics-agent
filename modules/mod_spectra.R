# modules/mod_spectra.R
#
# Spectra Explorer panel: MS-level / RT / precursor m/z filters (display
# only), spectrum selector + stick plot, TIC, BPC, spectrum metadata.

mod_spectra_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(3, 9),
    bslib::card(
      bslib::card_header("Filters"),
      bslib::card_body(
        shiny::checkboxGroupInput(ns("ms_level"), "MS level", choices = c(1, 2), selected = c(1, 2)),
        shiny::sliderInput(ns("rt_range"), "Retention time (s)", min = 0, max = 1, value = c(0, 1)),
        shiny::numericInput(ns("prec_min"), "Precursor m/z min", value = NA),
        shiny::numericInput(ns("prec_max"), "Precursor m/z max", value = NA),
        shiny::numericInput(ns("spectrum_index"), "Spectrum index", value = 1, min = 1, step = 1)
      )
    ),
    bslib::card(
      bslib::card_header("Spectra"),
      bslib::card_body(
        bslib::navset_tab(
          bslib::nav_panel("Stick Plot", shiny::plotOutput(ns("stick_plot")), shiny::uiOutput(ns("spectrum_meta"))),
          bslib::nav_panel("TIC", shiny::plotOutput(ns("tic_plot"))),
          bslib::nav_panel("BPC", shiny::plotOutput(ns("bpc_plot"))),
          bslib::nav_panel("Table", shiny::tableOutput(ns("spectra_table")))
        )
      )
    )
  )
}

mod_spectra_server <- function(id, shared, ctx) {
  shiny::moduleServer(id, function(input, output, session) {

    current_spectra <- shiny::reactive({
      shiny::req(shared$spectra_id)
      provenance_get_object(shared$store, shared$spectra_id)
    })

    shiny::observeEvent(current_spectra(), {
      sp <- current_spectra()
      if (length(sp) == 0) return(invisible(NULL))
      rt <- Spectra::rtime(sp)
      shiny::updateSliderInput(session, "rt_range", min = floor(min(rt, na.rm = TRUE)),
                                max = ceiling(max(rt, na.rm = TRUE)), value = range(rt, na.rm = TRUE))
      shiny::updateNumericInput(session, "spectrum_index", max = length(sp))
    })

    filtered_spectra <- shiny::reactive({
      sp <- current_spectra()
      levels_sel <- as.integer(input$ms_level %||% c(1, 2))
      filter_spectra(sp, ms_level = levels_sel,
                      rt_min = input$rt_range[1], rt_max = input$rt_range[2],
                      precursor_mz_min = if (!is.na(input$prec_min %||% NA)) input$prec_min else NULL,
                      precursor_mz_max = if (!is.na(input$prec_max %||% NA)) input$prec_max else NULL)
    })

    output$stick_plot <- shiny::renderPlot({
      sp <- filtered_spectra()
      shiny::req(length(sp) > 0)
      idx <- min(max(1, input$spectrum_index %||% 1), length(sp))
      plot_spectrum(sp, idx)
    })

    output$spectrum_meta <- shiny::renderUI({
      sp <- filtered_spectra()
      shiny::req(length(sp) > 0)
      idx <- min(max(1, input$spectrum_index %||% 1), length(sp))
      one <- sp[idx]
      htmltools::tags$p(sprintf("MS level: %s | RT: %.1f s | Precursor m/z: %s | Peaks: %d",
        Spectra::msLevel(one), Spectra::rtime(one),
        tryCatch(round(Spectra::precursorMz(one), 4), error = function(e) "NA"),
        Spectra::lengths(one)))
    })

    output$tic_plot <- shiny::renderPlot({
      sp <- filtered_spectra()
      shiny::req(length(sp) > 0)
      lvl <- if (1 %in% (input$ms_level %||% 1)) 1L else as.integer(input$ms_level[1])
      plot_tic(sp, ms_level = lvl)
    })

    output$bpc_plot <- shiny::renderPlot({
      sp <- filtered_spectra()
      shiny::req(length(sp) > 0)
      lvl <- if (1 %in% (input$ms_level %||% 1)) 1L else as.integer(input$ms_level[1])
      plot_bpc(sp, ms_level = lvl)
    })

    output$spectra_table <- shiny::renderTable({
      sp <- filtered_spectra()
      shiny::req(length(sp) > 0)
      n <- min(length(sp), 200)
      data.frame(
        index = seq_len(n),
        ms_level = Spectra::msLevel(sp)[seq_len(n)],
        rtime_s = round(Spectra::rtime(sp)[seq_len(n)], 2),
        n_peaks = Spectra::lengths(sp)[seq_len(n)]
      )
    })
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
