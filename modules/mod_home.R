# modules/mod_home.R
#
# Home panel: project overview, Claude connection status, LLM mode,
# model, Bioconductor version, execution environment, demo-data button.

mod_home_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(7, 5),
    bslib::card(
      bslib::card_header(htmltools::tags$span(htmltools::tags$i(class = "fa-solid fa-dna me-2"), "Bioconductor Proteomics Agent")),
      bslib::card_body(
        htmltools::tags$p(
          "A multi-agent, Claude-powered assistant for opening, inspecting, processing, ",
          "visualizing, and reporting protein mass-spectrometry data using R and Bioconductor. ",
          "Every scientific calculation is performed by deterministic R/Bioconductor functions; ",
          "Claude only plans and selects from a restricted set of registered tools."
        ),
        htmltools::tags$ul(
          class = "feature-list",
          htmltools::tags$li(htmltools::tags$i(class = "fa-solid fa-wave-square"), "Spectra / MsExperiment import from mzML, mzXML, and MGF"),
          htmltools::tags$li(htmltools::tags$i(class = "fa-solid fa-vial-circle-check"), "Quality control, spectrum processing, identification, and quantification"),
          htmltools::tags$li(htmltools::tags$i(class = "fa-solid fa-diagram-project"), "A bounded, auditable multi-agent tool-use loop with a full trace"),
          htmltools::tags$li(htmltools::tags$i(class = "fa-solid fa-file-circle-check"), "Reproducible HTML reports with provenance")
        ),
        shiny::actionButton(ns("load_demo"), htmltools::tagList(htmltools::tags$i(class = "fa-solid fa-flask"), " Load Demo Data"), class = "btn-primary"),
        shiny::uiOutput(ns("demo_status"))
      )
    ),
    bslib::card(
      bslib::card_header(htmltools::tags$span(htmltools::tags$i(class = "fa-solid fa-satellite-dish me-2"), "System Status")),
      bslib::card_body(
        shiny::uiOutput(ns("status_table")),
        shiny::actionButton(ns("check_connection"), htmltools::tagList(htmltools::tags$i(class = "fa-solid fa-plug-circle-check"), " Check Claude Connection"), class = "btn-outline-secondary btn-sm mt-2")
      )
    )
  )
}

mod_home_server <- function(id, shared, ctx) {
  shiny::moduleServer(id, function(input, output, session) {
    conn_status <- shiny::reactiveVal(shiny::isolate(claude_connection_status(shared$cfg, probe = FALSE)))

    shiny::observeEvent(input$check_connection, {
      shiny::withProgress(message = "Checking Claude connectivity...", {
        conn_status(claude_connection_status(shared$cfg, probe = TRUE))
      })
    })

    output$status_table <- shiny::renderUI({
      cs <- conn_status()
      docker_env <- Sys.getenv("RUNNING_IN_DOCKER", unset = "false")
      rows <- list(
        c("LLM mode", shared$cfg$llm_mode),
        c("Claude model", shared$cfg$anthropic_model),
        c("Claude status", cs$label),
        c("Bioconductor version", get_bioc_version() %||% "unknown"),
        c("R version", R.version.string),
        c("Execution environment", if (identical(docker_env, "true")) "Docker container" else "Local (non-Docker)"),
        c("Max agent steps", shared$cfg$max_agent_steps),
        c("Max upload size (MB)", shared$cfg$max_upload_mb)
      )
      badge_class <- if (isTRUE(cs$ok)) "status-badge-ok" else "status-badge-warn"
      htmltools::tagList(
        htmltools::tags$table(
          class = "table table-sm",
          htmltools::tags$tbody(
            lapply(rows, function(r) htmltools::tags$tr(htmltools::tags$td(htmltools::tags$strong(r[1])), htmltools::tags$td(as.character(r[2]))))
          )
        ),
        htmltools::tags$p(class = badge_class, cs$detail)
      )
    })

    shiny::observeEvent(input$load_demo, {
      shiny::withProgress(message = "Loading demo data...", value = 0, {
        res <- tryCatch(
          load_demo_dataset(shared, ctx, on_stage = function(label) shiny::incProgress(1 / 3, detail = label)),
          error = function(e) list(ok = FALSE, message = conditionMessage(e))
        )
        output$demo_status <- shiny::renderUI({
          if (isTRUE(res$ok)) {
            htmltools::tags$p(class = "status-badge-ok", "Demo data loaded: mzML spectra, PSM table, and abundance table are ready. See the other tabs.")
          } else {
            htmltools::tags$p(class = "status-badge-warn", paste("Failed to load demo data:", res$message))
          }
        })
      })
    })
  })
}

#' Load bundled demo files (mzML, PSM CSV, abundance CSV) into the shared
#' session state, exactly as if the user had uploaded them. `on_stage`, if
#' given, is called with a short label before each of the three imports so
#' the caller can drive a staged progress indicator.
#' @export
load_demo_dataset <- function(shared, ctx, on_stage = NULL) {
  demo_dir <- "inst/extdata"
  stage <- function(label) if (!is.null(on_stage)) on_stage(label)

  mzml_path <- file.path(demo_dir, "demo_lcmsms.mzML")
  psm_path <- file.path(demo_dir, "demo_psm_table.csv")
  quant_path <- file.path(demo_dir, "demo_abundance_table.csv")
  meta_path <- file.path(demo_dir, "demo_sample_metadata.csv")

  if (!file.exists(mzml_path)) stop("Demo mzML file not found at ", mzml_path)

  # --- Spectra ---
  stage("Importing mzML spectra...")
  file_id <- register_upload(shared$uploads_env, mzml_path, "demo_lcmsms.mzML")
  t0 <- Sys.time()
  imported <- import_ms_file(mzml_path, "demo_lcmsms.mzML")
  sp_id <- paste0("spectra_demo")
  provenance_put_object(shared$store, sp_id, imported$spectra)
  shared$spectra_id <- sp_id
  shared$source_filename <- "demo_lcmsms.mzML"
  shared$file_meta <- inspect_uploaded_file(mzml_path, "demo_lcmsms.mzML")
  provenance_add_entry(shared$store, agent = "data_intake", objective = "Load demo data",
    plan_step = NA_integer_, tool = "import_ms_file", reason = "User requested demo data.",
    arguments = list(file = "demo_lcmsms.mzML"), r_function = "import_ms_file",
    output_id = sp_id, status = "ok", duration_s = as.numeric(difftime(Sys.time(), t0, units = "secs")))

  # --- PSM table ---
  stage("Importing PSM identification table...")
  if (file.exists(psm_path)) {
    psm_file_id <- register_upload(shared$uploads_env, psm_path, "demo_psm_table.csv")
    df <- import_psm_table(psm_path)
    val <- validate_psm_table(df)
    if (val$ok) {
      psm_id <- "psm_demo"
      provenance_put_object(shared$store, psm_id, val$mapped_df)
      shared$psm_id <- psm_id
      shared$psm_column_map <- val$column_map
      provenance_add_entry(shared$store, agent = "identification", objective = "Load demo data",
        plan_step = NA_integer_, tool = "import_psm_table", reason = "User requested demo data.",
        arguments = list(file = "demo_psm_table.csv"), r_function = "import_psm_table",
        output_id = psm_id, status = "ok")
    }
  }

  # --- FASTA database ---
  stage("Importing demo protein FASTA...")
  fasta_path <- file.path(demo_dir, "demo_proteins.fasta")
  if (file.exists(fasta_path)) {
    fasta_file_id <- register_upload(shared$uploads_env, fasta_path, "demo_proteins.fasta")
    aa <- import_fasta_database(fasta_path)
    val <- validate_fasta_database(aa)
    if (val$ok) {
      fasta_id <- "fasta_demo"
      provenance_put_object(shared$store, fasta_id, aa)
      shared$fasta_id <- fasta_id
      provenance_add_entry(shared$store, agent = "identification", objective = "Load demo data",
        plan_step = NA_integer_, tool = "import_fasta_database", reason = "User requested demo data.",
        arguments = list(file = "demo_proteins.fasta"), r_function = "import_fasta_database",
        output_id = fasta_id, status = "ok")
    }
  }

  # --- Quant table ---
  stage("Importing abundance table...")
  if (file.exists(quant_path)) {
    quant_file_id <- register_upload(shared$uploads_env, quant_path, "demo_abundance_table.csv")
    df <- import_quant_table(quant_path)
    val <- validate_quant_table(df, id_col = colnames(df)[1])
    if (val$ok) {
      table_id <- "quanttable_demo"
      provenance_put_object(shared$store, table_id, df)
      shared$quant_table_id <- table_id
      shared$quant_id_col <- val$id_col
      shared$quant_sample_cols <- val$sample_cols
      provenance_add_entry(shared$store, agent = "quantification", objective = "Load demo data",
        plan_step = NA_integer_, tool = "import_quant_table", reason = "User requested demo data.",
        arguments = list(file = "demo_abundance_table.csv"), r_function = "import_quant_table",
        output_id = table_id, status = "ok")
    }
  }

  list(ok = TRUE)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
