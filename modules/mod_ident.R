# modules/mod_ident.R
#
# Identifications panel: PSM table upload, delimiter detection, column
# mapping, score/FDR filters, PSM table, peptide/protein summaries.

mod_ident_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    bslib::card(
      bslib::card_header("PSM Table"),
      bslib::card_body(
        shiny::fileInput(ns("file"), "Upload PSM table (CSV/TSV)", accept = c(".csv", ".tsv", ".txt")),
        shiny::uiOutput(ns("delim_map_info")),
        shiny::numericInput(ns("min_score"), "Minimum score", value = NA),
        shiny::sliderInput(ns("max_qvalue"), "Maximum q-value / FDR", min = 0, max = 1, value = 1, step = 0.01),
        shiny::actionButton(ns("apply_filter"), "Apply Filter", class = "btn-primary"),
        shiny::uiOutput(ns("validation_msg"))
      )
    ),
    bslib::card(
      bslib::card_header("Results"),
      bslib::card_body(
        bslib::navset_tab(
          bslib::nav_panel("PSM Table", shiny::tableOutput(ns("psm_table"))),
          bslib::nav_panel("Peptide Summary", shiny::tableOutput(ns("peptide_summary"))),
          bslib::nav_panel("Protein Summary", shiny::tableOutput(ns("protein_summary")))
        ),
        htmltools::tags$hr(),
        shiny::uiOutput(ns("summary_stats"))
      )
    )
  )
}

mod_ident_server <- function(id, shared, ctx) {
  shiny::moduleServer(id, function(input, output, session) {

    raw_df <- shiny::reactiveVal(NULL)
    filtered_df <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$file, {
      f <- input$file
      val <- validate_table_upload(f$name, f$size, shared$cfg$max_upload_mb)
      if (!val$ok) {
        output$validation_msg <- shiny::renderUI(htmltools::tags$p(class = "status-badge-warn", val$reason))
        return(invisible(NULL))
      }
      dest <- file.path(shared$session_dir, val$safe_name)
      file.copy(f$datapath, dest, overwrite = TRUE)
      assert_within_dir(dest, shared$session_dir)

      result <- tryCatch({
        df <- import_psm_table(dest)
        v <- validate_psm_table(df)
        if (!v$ok) stop(paste(v$errors, collapse = " "))
        psm_id <- paste0("psm_", safe_filename(tools::file_path_sans_ext(f$name)))
        provenance_put_object(shared$store, psm_id, v$mapped_df)
        shared$psm_id <- psm_id
        shared$psm_column_map <- v$column_map
        raw_df(v$mapped_df)
        filtered_df(v$mapped_df)
        provenance_add_entry(shared$store, agent = "identification", objective = "Manual PSM upload",
          plan_step = NA_integer_, tool = "import_psm_table", reason = "User uploaded a PSM table.",
          arguments = list(file = f$name), r_function = "import_psm_table", output_id = psm_id, status = "ok")
        list(ok = TRUE, delimiter = attr(df, "detected_delimiter"), column_map = v$column_map, n_rows = v$n_rows)
      }, error = function(e) list(ok = FALSE, message = conditionMessage(e)))

      output$validation_msg <- shiny::renderUI({
        if (isTRUE(result$ok)) htmltools::tags$p(class = "status-badge-ok", sprintf("Imported %d PSM rows.", result$n_rows))
        else htmltools::tags$p(class = "status-badge-warn", paste("Import failed:", result$message))
      })
      output$delim_map_info <- shiny::renderUI({
        if (!isTRUE(result$ok)) return(NULL)
        htmltools::tags$div(
          htmltools::tags$p(sprintf("Detected delimiter: %s", result$delimiter)),
          htmltools::tags$p("Column mapping: ", paste(names(result$column_map), unlist(result$column_map), sep = " <- ", collapse = "; "))
        )
      })
    })

    shiny::observeEvent(input$apply_filter, {
      shiny::req(raw_df())
      out <- filter_psms(raw_df(),
                          min_score = if (!is.na(input$min_score %||% NA)) input$min_score else NULL,
                          max_qvalue = if (!is.null(input$max_qvalue) && input$max_qvalue < 1) input$max_qvalue else NULL)
      filtered_df(out)
      stage_id <- paste0(shared$psm_id, "_filtered")
      provenance_put_object(shared$store, stage_id, out)
      s <- summarize_identifications(out)
      shared$ident_summary <- s[c("n_psm", "n_unique_peptides", "n_unique_proteins", "median_psms_per_protein")]
      provenance_add_entry(shared$store, agent = "identification", objective = "Manual PSM filter",
        plan_step = NA_integer_, tool = "filter_psms", reason = "User applied score/FDR filter.",
        arguments = list(min_score = input$min_score, max_qvalue = input$max_qvalue),
        r_function = "filter_psms", input_id = shared$psm_id, output_id = stage_id, status = "ok")
    })

    output$psm_table <- shiny::renderTable({
      df <- filtered_df() %||% raw_df()
      shiny::req(df)
      utils::head(df, 100)
    })

    output$peptide_summary <- shiny::renderTable({
      df <- filtered_df() %||% raw_df()
      shiny::req(df)
      s <- summarize_identifications(df)
      utils::head(s$peptide_summary[order(-s$peptide_summary$Freq), ], 50)
    })

    output$protein_summary <- shiny::renderTable({
      df <- filtered_df() %||% raw_df()
      shiny::req(df)
      s <- summarize_identifications(df)
      utils::head(s$protein_summary[order(-s$protein_summary$Freq), ], 50)
    })

    output$summary_stats <- shiny::renderUI({
      df <- filtered_df() %||% raw_df()
      shiny::req(df)
      s <- summarize_identifications(df)
      htmltools::tags$p(sprintf("%d PSMs | %d unique peptides | %d unique proteins",
                                 s$n_psm, s$n_unique_peptides, s$n_unique_proteins))
    })
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
