# modules/mod_quant.R
#
# Quantification panel: abundance-table upload, sample metadata,
# missingness, transformation, normalization, peptide-to-protein
# aggregation, PCA, optional two-group comparison.

mod_quant_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    bslib::card(
      bslib::card_header("Abundance Table"),
      bslib::card_body(
        shiny::fileInput(ns("file"), "Upload abundance table (CSV/TSV)", accept = c(".csv", ".tsv", ".txt")),
        shiny::uiOutput(ns("validation_msg")),
        shiny::fileInput(ns("metadata_file"), "Upload sample metadata (CSV, optional -- needs 'sample_id' + a group column)",
                          accept = c(".csv", ".tsv", ".txt")),
        shiny::uiOutput(ns("metadata_validation_msg")),
        htmltools::tags$hr(),
        shiny::selectInput(ns("id_col"), "Identifier column", choices = NULL),
        shiny::selectInput(ns("sample_cols"), "Sample columns", choices = NULL, multiple = TRUE),
        shiny::actionButton(ns("build_qf"), "Build QFeatures", class = "btn-primary"),
        htmltools::tags$hr(),
        shiny::selectInput(ns("norm_method"), "Normalization method",
                            choices = c("center.median", "center.mean", "quantiles", "vsn"), selected = "center.median"),
        shiny::actionButton(ns("run_normalize"), "Transform & Normalize"),
        htmltools::tags$hr(),
        shiny::textInput(ns("fcol"), "Protein id column (for aggregation)", value = "protein"),
        shiny::actionButton(ns("run_aggregate"), "Aggregate to Proteins"),
        htmltools::tags$hr(),
        shiny::selectInput(ns("group_col"), "Group column for comparison", choices = NULL),
        shiny::actionButton(ns("run_pca_btn"), "Run PCA / Comparison", class = "btn-secondary"),
        htmltools::tags$hr(),
        htmltools::tags$p(htmltools::tags$strong("Statistical Comparison (MSstats)")),
        shiny::selectInput(ns("msstats_protein_col"), "Protein id column", choices = NULL),
        shiny::actionButton(ns("run_msstats_btn"), "Run MSstats Comparison", class = "btn-secondary")
      )
    ),
    bslib::card(
      bslib::card_header("Results"),
      bslib::card_body(
        shiny::uiOutput(ns("missingness_ui")),
        htmltools::tags$hr(),
        shiny::plotOutput(ns("pca_plot")),
        htmltools::tags$hr(),
        shiny::plotOutput(ns("heatmap_plot")),
        htmltools::tags$hr(),
        shiny::tableOutput(ns("comparison_table")),
        htmltools::tags$hr(),
        htmltools::tags$p(htmltools::tags$strong("MSstats Comparison Result")),
        shiny::uiOutput(ns("msstats_summary_ui")),
        shiny::tableOutput(ns("msstats_table"))
      )
    )
  )
}

mod_quant_server <- function(id, shared, ctx) {
  shiny::moduleServer(id, function(input, output, session) {

    raw_df <- shiny::reactiveVal(NULL)
    sample_meta <- shiny::reactiveVal(NULL)
    qf_obj <- shiny::reactiveVal(NULL)
    active_assay <- shiny::reactiveVal(NULL)
    pca_res <- shiny::reactiveVal(NULL)
    cmp_res <- shiny::reactiveVal(NULL)
    synced_table_id <- shiny::reactiveVal(NULL)
    synced_qfeatures_id <- shiny::reactiveVal(NULL)
    synced_metadata_id <- shiny::reactiveVal(NULL)
    msstats_res <- shiny::reactiveVal(NULL)

    .default_protein_col <- function(cols, id_col) if ("protein" %in% cols) "protein" else id_col

    # shared$quant_table_id / shared$qfeatures_id are set not just by this
    # panel's own upload/build actions, but also by Home's "Load Demo Data"
    # and by the Agent Workspace's quantification tool calls -- without
    # this, the tab stays empty after either of those even though a table
    # or QFeatures object genuinely exists in the session.
    shiny::observeEvent(shared$quant_table_id, {
      table_id <- shared$quant_table_id
      shiny::req(table_id)
      if (identical(table_id, synced_table_id())) return(invisible(NULL))
      df <- provenance_get_object(shared$store, table_id)
      if (!is.null(df)) {
        raw_df(df)
        synced_table_id(table_id)
        id_col <- shared$quant_id_col %||% colnames(df)[1]
        sample_cols <- shared$quant_sample_cols %||% character(0)
        shiny::updateSelectInput(session, "id_col", choices = colnames(df), selected = id_col)
        shiny::updateSelectInput(session, "sample_cols", choices = colnames(df), selected = sample_cols)
        shiny::updateSelectInput(session, "msstats_protein_col", choices = colnames(df),
                                  selected = .default_protein_col(colnames(df), id_col))
      }
    }, ignoreNULL = TRUE)

    # shared$quant_metadata_id is set not just by this panel's own metadata
    # upload, but also by the Agent Workspace's import_sample_metadata tool
    # calls -- without this, a sample metadata table imported by the agent
    # wouldn't be usable for a manually-triggered MSstats run.
    shiny::observeEvent(shared$quant_metadata_id, {
      metadata_id <- shared$quant_metadata_id
      shiny::req(metadata_id)
      if (identical(metadata_id, synced_metadata_id())) return(invisible(NULL))
      meta <- provenance_get_object(shared$store, metadata_id)
      if (!is.null(meta)) {
        sample_meta(meta)
        synced_metadata_id(metadata_id)
      }
    }, ignoreNULL = TRUE)

    shiny::observeEvent(shared$qfeatures_id, {
      qfeatures_id <- shared$qfeatures_id
      shiny::req(qfeatures_id)
      if (identical(qfeatures_id, synced_qfeatures_id())) return(invisible(NULL))
      qf <- provenance_get_object(shared$store, qfeatures_id)
      assay_name <- shared$quant_assay_name
      if (!is.null(qf) && !is.null(assay_name) && assay_name %in% names(qf)) {
        qf_obj(qf)
        active_assay(assay_name)
        synced_qfeatures_id(qfeatures_id)
        cd <- as.data.frame(SummarizedExperiment::colData(qf[[assay_name]]))
        shiny::updateSelectInput(session, "group_col", choices = colnames(cd))
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
        df <- import_quant_table(dest)
        v <- validate_quant_table(df)
        if (!v$ok) stop(paste(v$errors, collapse = " "))
        table_id <- paste0("quanttable_", safe_filename(tools::file_path_sans_ext(f$name)))
        provenance_put_object(shared$store, table_id, df)
        shared$quant_table_id <- table_id
        raw_df(df)
        synced_table_id(table_id)
        shiny::updateSelectInput(session, "id_col", choices = colnames(df), selected = v$id_col)
        shiny::updateSelectInput(session, "sample_cols", choices = v$sample_cols, selected = v$sample_cols)
        shiny::updateSelectInput(session, "msstats_protein_col", choices = colnames(df),
                                  selected = .default_protein_col(colnames(df), v$id_col))
        provenance_add_entry(shared$store, agent = "quantification", objective = "Manual quant upload",
          plan_step = NA_integer_, tool = "import_quant_table", reason = "User uploaded an abundance table.",
          arguments = list(file = f$name), r_function = "import_quant_table", output_id = table_id, status = "ok")
        list(ok = TRUE, n_rows = nrow(df))
      }, error = function(e) list(ok = FALSE, message = conditionMessage(e)))

      output$validation_msg <- shiny::renderUI({
        if (isTRUE(result$ok)) htmltools::tags$p(class = "status-badge-ok", sprintf("Imported %d rows.", result$n_rows))
        else htmltools::tags$p(class = "status-badge-warn", paste("Import failed:", result$message))
      })
    })

    shiny::observeEvent(input$metadata_file, {
      f <- input$metadata_file
      val <- validate_table_upload(f$name, f$size, shared$cfg$max_upload_mb)
      if (!val$ok) {
        output$metadata_validation_msg <- shiny::renderUI(htmltools::tags$p(class = "status-badge-warn", val$reason))
        return(invisible(NULL))
      }
      dest <- file.path(shared$session_dir, val$safe_name)
      file.copy(f$datapath, dest, overwrite = TRUE)
      assert_within_dir(dest, shared$session_dir)

      result <- tryCatch({
        df <- utils::read.csv(dest, stringsAsFactors = FALSE, check.names = FALSE)
        v <- validate_sample_metadata(df)
        if (!v$ok) stop(paste(v$errors, collapse = " "))
        metadata_id <- paste0("samplemeta_", safe_filename(tools::file_path_sans_ext(f$name)))
        provenance_put_object(shared$store, metadata_id, df)
        shared$quant_metadata_id <- metadata_id
        sample_meta(df)
        synced_metadata_id(metadata_id)
        provenance_add_entry(shared$store, agent = "quantification", objective = "Manual sample metadata upload",
          plan_step = NA_integer_, tool = "import_sample_metadata", reason = "User uploaded sample metadata.",
          arguments = list(file = f$name), r_function = "import_sample_metadata", output_id = metadata_id, status = "ok")
        list(ok = TRUE, n_rows = nrow(df))
      }, error = function(e) list(ok = FALSE, message = conditionMessage(e)))

      output$metadata_validation_msg <- shiny::renderUI({
        if (isTRUE(result$ok)) htmltools::tags$p(class = "status-badge-ok", sprintf("Imported %d sample metadata rows.", result$n_rows))
        else htmltools::tags$p(class = "status-badge-warn", paste("Metadata import failed:", result$message))
      })
    })

    shiny::observeEvent(input$build_qf, {
      shiny::req(raw_df(), input$id_col, input$sample_cols)
      result <- tryCatch({
        qf <- build_qfeatures(raw_df(), input$id_col, input$sample_cols, assay_name = "peptides",
                               sample_metadata = sample_meta())
        qf_id <- paste0(shared$quant_table_id, "_qf")
        provenance_put_object(shared$store, qf_id, qf)
        shared$qfeatures_id <- qf_id
        shared$quant_id_col <- input$id_col
        shared$quant_sample_cols <- input$sample_cols
        shared$quant_assay_name <- "peptides"
        qf_obj(qf)
        active_assay("peptides")
        synced_qfeatures_id(qf_id)
        cd <- as.data.frame(SummarizedExperiment::colData(qf[["peptides"]]))
        shiny::updateSelectInput(session, "group_col", choices = colnames(cd))
        provenance_add_entry(shared$store, agent = "quantification", objective = "Manual QFeatures build",
          plan_step = NA_integer_, tool = "build_qfeatures", reason = "User clicked Build QFeatures.",
          arguments = list(id_col = input$id_col, sample_cols = input$sample_cols),
          r_function = "build_qfeatures", input_id = shared$quant_table_id, output_id = qf_id, status = "ok")
        list(ok = TRUE)
      }, error = function(e) list(ok = FALSE, message = conditionMessage(e)))
      if (!isTRUE(result$ok)) {
        output$validation_msg <- shiny::renderUI(htmltools::tags$p(class = "status-badge-warn", paste("QFeatures build failed:", result$message)))
      }
    })

    shiny::observeEvent(input$run_normalize, {
      shiny::req(qf_obj(), active_assay())
      result <- tryCatch({
        qf <- transform_quantification(qf_obj(), active_assay(), method = "log2")
        log_assay <- paste0(active_assay(), "_log2")
        qf <- normalize_quantification(qf, log_assay, method = input$norm_method)
        norm_assay <- paste0(log_assay, "_norm")
        qf_obj(qf)
        active_assay(norm_assay)
        qf_id <- paste0(shared$qfeatures_id, "_norm")
        provenance_put_object(shared$store, qf_id, qf)
        shared$qfeatures_id <- qf_id
        shared$quant_assay_name <- norm_assay
        synced_qfeatures_id(qf_id)
        provenance_add_entry(shared$store, agent = "quantification", objective = "Manual transform/normalize",
          plan_step = NA_integer_, tool = "normalize_quantification", reason = "User clicked Transform & Normalize.",
          arguments = list(method = input$norm_method), r_function = "normalize_quantification",
          output_id = qf_id, status = "ok")
        list(ok = TRUE)
      }, error = function(e) list(ok = FALSE, message = conditionMessage(e)))
      if (!isTRUE(result$ok)) {
        output$validation_msg <- shiny::renderUI(htmltools::tags$p(class = "status-badge-warn", paste("Normalization failed:", result$message)))
      }
    })

    shiny::observeEvent(input$run_aggregate, {
      shiny::req(qf_obj(), active_assay())
      result <- tryCatch({
        qf <- aggregate_to_proteins(qf_obj(), active_assay(), fcol = input$fcol %||% "protein")
        new_assay <- paste0(active_assay(), "_proteins")
        qf_obj(qf)
        active_assay(new_assay)
        qf_id <- paste0(shared$qfeatures_id, "_prot")
        provenance_put_object(shared$store, qf_id, qf)
        shared$qfeatures_id <- qf_id
        shared$quant_assay_name <- new_assay
        synced_qfeatures_id(qf_id)
        provenance_add_entry(shared$store, agent = "quantification", objective = "Manual aggregation",
          plan_step = NA_integer_, tool = "aggregate_to_proteins", reason = "User clicked Aggregate to Proteins.",
          arguments = list(fcol = input$fcol), r_function = "aggregate_to_proteins", output_id = qf_id, status = "ok")
        list(ok = TRUE)
      }, error = function(e) list(ok = FALSE, message = conditionMessage(e)))
      if (!isTRUE(result$ok)) {
        output$validation_msg <- shiny::renderUI(htmltools::tags$p(class = "status-badge-warn", paste("Aggregation failed:", result$message)))
      }
    })

    shiny::observeEvent(input$run_pca_btn, {
      shiny::req(qf_obj(), active_assay())
      p <- run_pca(qf_obj(), active_assay())
      pca_res(p)
      cmp <- NULL
      if (isTRUE(p$ok) && nzchar(input$group_col %||% "")) {
        cd <- SummarizedExperiment::colData(qf_obj()[[active_assay()]])
        if (input$group_col %in% colnames(cd)) {
          cmp <- run_two_group_comparison(qf_obj(), active_assay(), cd[[input$group_col]])
        }
      }
      cmp_res(cmp)
      shared$quant_summary <- list(
        assay = active_assay(), n_features = nrow(qf_obj()[[active_assay()]]),
        n_samples = ncol(qf_obj()[[active_assay()]]),
        pca_ok = isTRUE(p$ok), comparison_ok = isTRUE(cmp$ok %||% FALSE)
      )
      provenance_add_entry(shared$store, agent = "quantification", objective = "Manual exploratory analysis",
        plan_step = NA_integer_, tool = "run_exploratory_analysis", reason = "User clicked Run PCA / Comparison.",
        arguments = list(assay_name = active_assay(), group_column = input$group_col),
        r_function = "run_exploratory_analysis", status = if (isTRUE(p$ok)) "ok" else "error",
        warnings = if (!isTRUE(p$ok)) p$reason else character(0))
    })

    shiny::observeEvent(input$run_msstats_btn, {
      shiny::req(raw_df(), input$id_col, input$sample_cols, input$msstats_protein_col, sample_meta())
      shiny::validate(shiny::need(nzchar(input$group_col %||% ""), "Select a group column first (build QFeatures with sample metadata to populate it)."))
      res <- run_msstats_comparison(raw_df(), input$msstats_protein_col, input$id_col, input$sample_cols,
                                     sample_meta(), input$group_col)
      msstats_res(res)
      if (isTRUE(res$ok)) {
        shared$msstats_summary <- list(
          method = res$method, n_proteins = nrow(res$results),
          n_significant_padj05 = sum(res$results$adj.pvalue < 0.05, na.rm = TRUE)
        )
      }
      provenance_add_entry(shared$store, agent = "quantification", objective = "Manual MSstats comparison",
        plan_step = NA_integer_, tool = "run_msstats_comparison", reason = "User clicked Run MSstats Comparison.",
        arguments = list(protein_col = input$msstats_protein_col, id_col = input$id_col, group_col = input$group_col),
        r_function = "run_msstats_comparison", status = if (isTRUE(res$ok)) "ok" else "error",
        warnings = if (!isTRUE(res$ok)) res$reason else character(0))
    })

    output$missingness_ui <- shiny::renderUI({
      shiny::req(qf_obj(), active_assay())
      m <- summarize_missingness(qf_obj(), active_assay())
      htmltools::tags$p(sprintf("Active assay: %s | %d values, %.1f%% missing, %d features fully missing.",
                                 active_assay(), m$total_values, m$pct_missing, m$features_fully_missing))
    })

    output$pca_plot <- shiny::renderPlot({
      p <- pca_res()
      shiny::req(p, isTRUE(p$ok))
      ggplot2::ggplot(p$scores, ggplot2::aes(x = .data$PC1, y = .data$PC2, label = .data$sample_id)) +
        ggplot2::geom_point(size = 3, color = "#22d3ee") +
        ggplot2::geom_text(vjust = -1, color = "#b8c4d9") +
        ggplot2::labs(title = "PCA of samples",
                      x = sprintf("PC1 (%.1f%%)", 100 * p$variance_explained[1]),
                      y = sprintf("PC2 (%.1f%%)", 100 * (p$variance_explained[2] %||% 0))) +
        theme_bpa_dark()
    })

    output$comparison_table <- shiny::renderTable({
      cmp <- cmp_res()
      shiny::req(cmp, isTRUE(cmp$ok))
      utils::head(cmp$results[order(cmp$results$p.value), ], 50)
    })

    output$heatmap_plot <- shiny::renderPlot({
      input$run_pca_btn
      shiny::req(qf_obj(), active_assay())
      ht <- tryCatch(plot_quant_heatmap(qf_obj(), active_assay()), error = function(e) NULL)
      shiny::req(ht)
      ComplexHeatmap::draw(ht)
    })

    output$msstats_summary_ui <- shiny::renderUI({
      res <- msstats_res()
      shiny::req(res)
      if (!isTRUE(res$ok)) return(htmltools::tags$p(class = "status-badge-warn", paste("MSstats comparison failed:", res$reason)))
      n_sig <- sum(res$results$adj.pvalue < 0.05, na.rm = TRUE)
      htmltools::tags$p(class = "status-badge-ok",
                         sprintf("%s | %d proteins compared, %d significant (adj. p < 0.05).", res$method, nrow(res$results), n_sig))
    })

    output$msstats_table <- shiny::renderTable({
      res <- msstats_res()
      shiny::req(res, isTRUE(res$ok))
      utils::head(res$results, 50)
    })
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
