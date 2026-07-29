# modules/mod_ident.R
#
# Identifications panel: PSM table upload, delimiter detection, column
# mapping, score/FDR filters, PSM table, peptide/protein summaries.

mod_ident_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    bslib::card(
      bslib::card_header("Database Search (FASTA)"),
      bslib::card_body(
        htmltools::tags$p(class = "text-muted",
          "Approximate mass/fragment-matching search for exploratory use only — no decoy database, no FDR control. ",
          htmltools::tags$code("score"), " is the fraction of theoretical b/y ions matched, for ranking only; ",
          htmltools::tags$code("qvalue"), " is intentionally blank."),
        bslib::layout_columns(
          col_widths = c(6, 6),
          shiny::fileInput(ns("fasta_file"), "Upload protein FASTA", accept = c(".fasta", ".fa", ".fas")),
          shiny::uiOutput(ns("fasta_status"))
        ),
        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          shiny::selectInput(ns("enzyme"), "Enzyme", choices = list("Trypsin" = "trypsin")),
          shiny::numericInput(ns("missed_cleavages"), "Max missed cleavages", value = 1, min = 0, max = 2),
          shiny::sliderInput(ns("pep_len"), "Peptide length range", min = 5, max = 40, value = c(6, 25)),
          shiny::numericInput(ns("precursor_ppm"), "Precursor tolerance (ppm)", value = 10, min = 1, max = 50)
        ),
        shiny::checkboxGroupInput(ns("ptm_choices"), "Variable PTMs to search",
                                   choices = names(VARIABLE_PTMS), selected = "Oxidation (M)"),
        shiny::actionButton(ns("run_search"), "Run Database Search", class = "btn-primary"),
        shiny::uiOutput(ns("search_status"))
      )
    ),
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
  )
}

mod_ident_server <- function(id, shared, ctx) {
  shiny::moduleServer(id, function(input, output, session) {

    raw_df <- shiny::reactiveVal(NULL)
    filtered_df <- shiny::reactiveVal(NULL)
    synced_psm_id <- shiny::reactiveVal(NULL)
    synced_fasta_id <- shiny::reactiveVal(NULL)

    # shared$fasta_id may be set by this panel's own upload, Home's demo
    # loader, or the Agent Workspace -- keep the status line in sync with
    # whichever set it last, mirroring the shared$psm_id sync below.
    shiny::observeEvent(shared$fasta_id, {
      fasta_id <- shared$fasta_id
      shiny::req(fasta_id)
      if (identical(fasta_id, synced_fasta_id())) return(invisible(NULL))
      aa <- provenance_get_object(shared$store, fasta_id)
      if (!is.null(aa)) {
        synced_fasta_id(fasta_id)
        output$fasta_status <- shiny::renderUI(
          htmltools::tags$p(class = "status-badge-ok", sprintf("Loaded %d protein(s).", length(aa))))
      }
    }, ignoreNULL = TRUE)

    # shared$psm_id is set not just by this panel's own upload, but also by
    # Home's "Load Demo Data" and by the Agent Workspace's identification
    # tool calls -- without this, the tab stays empty after either of those
    # even though a PSM table genuinely exists in the session.
    shiny::observeEvent(shared$psm_id, {
      psm_id <- shared$psm_id
      shiny::req(psm_id)
      if (identical(psm_id, synced_psm_id())) return(invisible(NULL))
      df <- provenance_get_object(shared$store, psm_id)
      if (!is.null(df)) {
        raw_df(df)
        filtered_df(df)
        synced_psm_id(psm_id)
      }
    }, ignoreNULL = TRUE)

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
        synced_psm_id(psm_id)
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

    shiny::observeEvent(input$fasta_file, {
      f <- input$fasta_file
      val <- validate_fasta_upload(f$name, f$size, shared$cfg$max_upload_mb)
      if (!val$ok) {
        output$fasta_status <- shiny::renderUI(htmltools::tags$p(class = "status-badge-warn", val$reason))
        return(invisible(NULL))
      }
      dest <- file.path(shared$session_dir, val$safe_name)
      file.copy(f$datapath, dest, overwrite = TRUE)
      assert_within_dir(dest, shared$session_dir)

      result <- tryCatch({
        aa <- import_fasta_database(dest)
        v <- validate_fasta_database(aa)
        if (!v$ok) stop(paste(v$errors, collapse = " "))
        fasta_id <- paste0("fasta_", safe_filename(tools::file_path_sans_ext(f$name)))
        provenance_put_object(shared$store, fasta_id, aa)
        shared$fasta_id <- fasta_id
        synced_fasta_id(fasta_id)
        provenance_add_entry(shared$store, agent = "identification", objective = "Manual FASTA upload",
          plan_step = NA_integer_, tool = "import_fasta_database", reason = "User uploaded a FASTA database.",
          arguments = list(file = f$name), r_function = "import_fasta_database", output_id = fasta_id, status = "ok")
        list(ok = TRUE, n_proteins = v$n_proteins)
      }, error = function(e) list(ok = FALSE, message = conditionMessage(e)))

      output$fasta_status <- shiny::renderUI({
        if (isTRUE(result$ok)) htmltools::tags$p(class = "status-badge-ok", sprintf("Loaded %d protein(s).", result$n_proteins))
        else htmltools::tags$p(class = "status-badge-warn", paste("Import failed:", result$message))
      })
    })

    shiny::observeEvent(input$run_search, {
      if (is.null(shared$fasta_id) || is.null(shared$spectra_id)) {
        output$search_status <- shiny::renderUI(htmltools::tags$p(class = "status-badge-warn",
          "Upload a FASTA database and load spectra (Upload & Experiment tab, or Home's demo data) before running a search."))
        return(invisible(NULL))
      }
      aa <- provenance_get_object(shared$store, shared$fasta_id)
      sp <- provenance_get_object(shared$store, shared$spectra_id)

      result <- tryCatch({
        res <- run_fasta_search(aa, sp,
          missed_cleavages = 0:input$missed_cleavages,
          min_length = input$pep_len[1], max_length = input$pep_len[2],
          variable_ptms = input$ptm_choices, precursor_ppm = input$precursor_ppm)
        psm_id <- paste0("psm_fastasearch_", format(Sys.time(), "%H%M%OS3"))
        provenance_put_object(shared$store, psm_id, res$psm_df)
        shared$psm_id <- psm_id
        provenance_add_entry(shared$store, agent = "identification", objective = "FASTA database search",
          plan_step = NA_integer_, tool = "run_fasta_search", reason = "User ran a FASTA database search.",
          arguments = list(fasta_id = shared$fasta_id, spectra_id = shared$spectra_id,
                            variable_ptms = input$ptm_choices, precursor_ppm = input$precursor_ppm),
          r_function = "run_fasta_search", input_id = shared$fasta_id, output_id = psm_id, status = "ok")
        list(ok = TRUE, summary = res$summary)
      }, error = function(e) list(ok = FALSE, message = conditionMessage(e)))

      output$search_status <- shiny::renderUI({
        if (isTRUE(result$ok)) {
          s <- result$summary
          htmltools::tags$p(class = "status-badge-ok",
            sprintf("Searched %d candidate peptides -> %d PSM(s), %d unique peptide(s), %d unique protein(s).",
                     s$n_candidate_peptides, s$n_psm_matches, s$n_unique_peptides, s$n_unique_proteins))
        } else {
          htmltools::tags$p(class = "status-badge-warn", paste("Search failed:", result$message))
        }
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
