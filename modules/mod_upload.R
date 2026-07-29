# modules/mod_upload.R
#
# Upload & Experiment panel: file upload, format guidance, validation,
# metadata summary, Spectra/MsExperiment summary, sample metadata.

mod_upload_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    bslib::card(
      bslib::card_header("Upload"),
      bslib::card_body(
        shiny::fileInput(ns("file"), "Upload mzML / mzXML / MGF",
                          accept = c(".mzML", ".mzml", ".mzXML", ".mzxml", ".mgf")),
        htmltools::tags$p(class = "text-muted small",
          "Supported formats: mzML, mzXML, MGF. Proprietary vendor formats (Thermo RAW, Bruker, ",
          "SCIEX, Waters) are not read directly -- convert to mzML first (e.g. with ProteoWizard msconvert)."),
        htmltools::tags$p(class = "text-muted small",
          htmltools::tags$i(class = "fa-solid fa-clock"),
          " Real LC-MS/MS files take longer than the demo file to import --",
          " roughly 10-60 seconds for a few hundred to a few thousand spectra,",
          " depending on file size. This is normal; watch the progress panel."),
        shiny::uiOutput(ns("validation_msg"))
      )
    ),
    bslib::card(
      bslib::card_header("Experiment Summary"),
      bslib::card_body(
        shiny::uiOutput(ns("file_meta_ui")),
        htmltools::tags$hr(),
        shiny::uiOutput(ns("experiment_summary_ui")),
        htmltools::tags$hr(),
        shiny::tableOutput(ns("sample_metadata"))
      )
    )
  )
}

mod_upload_server <- function(id, shared, ctx) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observeEvent(input$file, {
      req <- input$file
      val <- validate_ms_upload(req$name, req$size, shared$cfg$max_upload_mb)
      if (!val$ok) {
        output$validation_msg <- shiny::renderUI(htmltools::tags$p(class = "status-badge-warn", val$reason))
        return(invisible(NULL))
      }
      dest <- file.path(shared$session_dir, val$safe_name)
      file.copy(req$datapath, dest, overwrite = TRUE)
      assert_within_dir(dest, shared$session_dir)

      # Real mzML files can take anywhere from a couple seconds to a minute
      # to parse depending on spectrum count, so this is staged into
      # distinct steps rather than one static "Importing..." spinner --
      # it gives the user a sense of progress even though the big mzR
      # parse itself is a single opaque call we can't subdivide further.
      shiny::withProgress(message = "Importing spectra...", value = 0, {
        result <- tryCatch({
          t0 <- Sys.time()
          shiny::incProgress(0.15, detail = "Computing checksum...")
          file_meta <- inspect_uploaded_file(dest, req$name)
          shiny::incProgress(0.25, detail = "Parsing spectra (large real files can take up to a minute)...")
          imported <- import_ms_file(dest, req$name)
          shiny::incProgress(0.5, detail = "Recording provenance...")
          sp_id <- paste0("spectra_", safe_filename(tools::file_path_sans_ext(req$name)))
          provenance_put_object(shared$store, sp_id, imported$spectra)
          shared$spectra_id <- sp_id
          shared$source_filename <- req$name
          shared$file_meta <- file_meta
          provenance_add_entry(shared$store, agent = "data_intake", objective = "Manual upload",
            plan_step = NA_integer_, tool = "import_ms_file", reason = "User uploaded a file via the Upload panel.",
            arguments = list(file = req$name), r_function = "import_ms_file",
            output_id = sp_id, status = "ok", duration_s = as.numeric(difftime(Sys.time(), t0, units = "secs")))
          shiny::incProgress(0.1, detail = "Done.")
          list(ok = TRUE)
        }, error = function(e) list(ok = FALSE, message = conditionMessage(e)))
      })

      output$validation_msg <- shiny::renderUI({
        if (isTRUE(result$ok)) {
          htmltools::tags$p(class = "status-badge-ok", sprintf("Imported '%s' successfully.", req$name))
        } else {
          htmltools::tags$p(class = "status-badge-warn", paste("Import failed:", result$message))
        }
      })
    })

    output$file_meta_ui <- shiny::renderUI({
      if (is.null(shared$file_meta)) return(htmltools::tags$p(class = "text-muted", "No file imported yet."))
      m <- shared$file_meta
      htmltools::tags$table(class = "table table-sm",
        htmltools::tags$tbody(
          htmltools::tags$tr(htmltools::tags$td(htmltools::tags$strong("Filename")), htmltools::tags$td(m$original_filename)),
          htmltools::tags$tr(htmltools::tags$td(htmltools::tags$strong("Detected format")), htmltools::tags$td(m$detected_format)),
          htmltools::tags$tr(htmltools::tags$td(htmltools::tags$strong("Size (bytes)")), htmltools::tags$td(format(m$size_bytes, big.mark = ","))),
          htmltools::tags$tr(htmltools::tags$td(htmltools::tags$strong("SHA-256")), htmltools::tags$td(htmltools::tags$code(m$sha256)))
        )
      )
    })

    output$experiment_summary_ui <- shiny::renderUI({
      if (is.null(shared$spectra_id)) return(htmltools::tags$p(class = "text-muted", "No Spectra/MsExperiment created yet."))
      sp <- provenance_get_object(shared$store, shared$spectra_id)
      s <- summarize_experiment(sp)
      htmltools::tags$table(class = "table table-sm",
        htmltools::tags$tbody(
          htmltools::tags$tr(htmltools::tags$td(htmltools::tags$strong("Total spectra")), htmltools::tags$td(s$n_spectra)),
          htmltools::tags$tr(htmltools::tags$td(htmltools::tags$strong("MS level counts")), htmltools::tags$td(paste(names(s$ms_level_counts), unlist(s$ms_level_counts), sep = "=", collapse = ", "))),
          htmltools::tags$tr(htmltools::tags$td(htmltools::tags$strong("RT range (s)")), htmltools::tags$td(paste(round(s$rt_range_s, 1), collapse = " - ")))
        )
      )
    })

    output$sample_metadata <- shiny::renderTable({
      if (is.null(shared$spectra_id)) return(NULL)
      data.frame(sample_id = shared$source_filename %||% "unknown", sample_name = shared$source_filename %||% "unknown")
    })
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
