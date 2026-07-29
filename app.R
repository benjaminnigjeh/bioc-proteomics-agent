# app.R
#
# Bioconductor Proteomics Agent -- main Shiny application entry point.
# Sources R/ (deterministic science + agent infrastructure) and modules/
# (one Shiny module per UI section), then assembles a bslib navbar app.

suppressWarnings({
  r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
  for (f in r_files) source(f, local = FALSE)

  module_files <- list.files("modules", pattern = "\\.R$", full.names = TRUE)
  for (f in module_files) source(f, local = FALSE)
})

assert_registry_is_safe()

cfg <- validate_app_config(get_app_config())

# A dark, "AI operations console" theme: near-black base, cyan/violet
# accent pair, a geometric display face for headings and a monospace face
# for code/trace/log-style content -- layered with www/custom.css for the
# glow, gradient, and grid-pattern details bslib's Sass variables can't
# express directly.
app_theme <- bslib::bs_theme(
  version = 5,
  bg = "#090c14",
  fg = "#dbe4f0",
  primary = "#22d3ee",
  secondary = "#8b5cf6",
  success = "#34d399",
  info = "#38bdf8",
  warning = "#fbbf24",
  danger = "#f87171",
  base_font = bslib::font_google("Inter"),
  heading_font = bslib::font_google("Space Grotesk"),
  code_font = bslib::font_google("JetBrains Mono"),
  "navbar-bg" = "#0b0f1c",
  "body-secondary-bg" = "#0f1422",
  "border-radius" = "0.9rem",
  "border-radius-lg" = "1.1rem"
)

ui <- bslib::page_navbar(
  title = htmltools::tagList(
    htmltools::tags$span(class = "brand-mark", htmltools::tags$i(class = "fa-solid fa-microchip")),
    htmltools::tags$span(class = "brand-text", "Bioconductor Proteomics Agent")
  ),
  id = "main_nav",
  theme = app_theme,
  header = htmltools::tags$head(
    htmltools::tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    htmltools::tags$meta(name = "color-scheme", content = "dark")
  ),
  footer = htmltools::tags$div(
    class = "app-footer",
    htmltools::tags$i(class = "fa-solid fa-triangle-exclamation"),
    htmltools::tags$strong("Research demonstration only. Not intended for clinical diagnosis or clinical decision-making.")
  ),
  bslib::nav_panel("Home", mod_home_ui("home")),
  bslib::nav_panel("Upload & Experiment", mod_upload_ui("upload")),
  bslib::nav_panel("Spectra Explorer", mod_spectra_ui("spectra")),
  bslib::nav_panel("Quality Control", mod_qc_ui("qc")),
  bslib::nav_panel("Identifications", mod_ident_ui("ident")),
  bslib::nav_panel("Quantification", mod_quant_ui("quant")),
  bslib::nav_panel("Agent Workspace", mod_agent_ui("agent")),
  bslib::nav_panel("Report", mod_report_ui("report"))
)

server <- function(input, output, session) {
  session_dir <- create_session_dir(session_id = session$token)
  shared <- shiny::reactiveValues(
    cfg = cfg,
    store = new_provenance_store(),
    session_dir = session_dir,
    uploads_env = new.env(),
    spectra_id = NULL,
    ms_experiment_id = NULL,
    file_meta = NULL,
    source_filename = NULL,
    qc_metrics = NULL,
    processing_stage_id = NULL,
    processing_comparison = NULL,
    psm_id = NULL,
    psm_column_map = NULL,
    ident_summary = NULL,
    quant_table_id = NULL,
    quant_id_col = NULL,
    quant_sample_cols = NULL,
    qfeatures_id = NULL,
    quant_assay_name = NULL,
    quant_summary = NULL,
    plan = NULL,
    trace_bump = 0L,
    final_narrative = NULL,
    agent_status = "idle",
    report_path = NULL,
    stop_requested = FALSE
  )

  # shared$store / shared$uploads_env are set once above and never change
  # again for the life of the session, so isolate() is safe here -- this
  # is a one-time read at session start, outside any reactive consumer,
  # which reactiveValues otherwise forbids.
  ctx <- shiny::isolate(new_session_ctx(
    store = shared$store,
    session_dir = session_dir,
    uploads_env = shared$uploads_env,
    report_params_builder = function(objective) build_report_params(shared, objective)
  ))

  shiny::onSessionEnded(function() {
    cleanup_session_dir(session_dir)
  })

  mod_home_server("home", shared, ctx)
  mod_upload_server("upload", shared, ctx)
  mod_spectra_server("spectra", shared, ctx)
  mod_qc_server("qc", shared, ctx)
  mod_ident_server("ident", shared, ctx)
  mod_quant_server("quant", shared, ctx)
  mod_agent_server("agent", shared, ctx)
  mod_report_server("report", shared, ctx)
}

shiny::shinyApp(ui = ui, server = server)
