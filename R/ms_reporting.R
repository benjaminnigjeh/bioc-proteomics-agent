# R/ms_reporting.R
#
# Deterministic report generation for the Reporting Agent. Renders the
# R Markdown template in reports/report_template.Rmd to a self-contained
# HTML file, and produces provenance/QC/result export files.

#' Render the analysis report to HTML.
#'
#' @param params list of parameters passed to the Rmd (objective, metadata,
#'   qc, processing comparison, identifications, quantification, plan,
#'   tool calls, trace data.frame, warnings, session info, etc.)
#' @param output_dir directory to write the report into (already validated,
#'   session-scoped)
#' @param template_path path to the .Rmd template
#' @export
generate_report <- function(params, output_dir, template_path = "reports/report_template.Rmd") {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) stop("rmarkdown package required.")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  out_file <- file.path(output_dir, sprintf("bioc_proteomics_report_%s.html",
                                             format(Sys.time(), "%Y%m%d_%H%M%S")))

  rmarkdown::render(
    input = template_path,
    output_file = out_file,
    params = params,
    envir = new.env(),
    quiet = TRUE
  )

  out_file
}

#' Write the QC metrics table to CSV.
#' @export
export_qc_table <- function(qc_metrics, output_dir) {
  flat <- data.frame(
    metric = c("n_spectra", "n_ms1", "n_ms2", "ms2_ms1_ratio",
               "rt_min_s", "rt_max_s", "n_missing_precursor", "n_empty_spectra"),
    value = c(qc_metrics$n_spectra, qc_metrics$n_ms1, qc_metrics$n_ms2,
              qc_metrics$ms2_ms1_ratio, qc_metrics$rt_range_s[1], qc_metrics$rt_range_s[2],
              qc_metrics$n_missing_precursor, qc_metrics$n_empty_spectra),
    stringsAsFactors = FALSE
  )
  path <- file.path(output_dir, "qc_metrics.csv")
  utils::write.csv(flat, path, row.names = FALSE)
  path
}

#' Write a processed results table (e.g. before/after comparison) to CSV.
#' @export
export_processed_table <- function(df_or_list, output_dir, filename = "processed_results.csv") {
  path <- file.path(output_dir, filename)
  if (is.data.frame(df_or_list)) {
    utils::write.csv(df_or_list, path, row.names = FALSE)
  } else {
    flat <- data.frame(key = names(unlist(df_or_list)), value = unlist(df_or_list), stringsAsFactors = FALSE)
    utils::write.csv(flat, path, row.names = FALSE)
  }
  path
}

#' Write the full provenance log to JSON.
#' @export
export_provenance_json <- function(store, output_dir) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite package required.")
  path <- file.path(output_dir, "provenance.json")
  jsonlite::write_json(provenance_as_list(store), path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  path
}

#' Write the agent trace table to CSV.
#' @export
export_trace_csv <- function(store, output_dir) {
  path <- file.path(output_dir, "agent_trace.csv")
  utils::write.csv(provenance_as_dataframe(store), path, row.names = FALSE)
  path
}

#' Build the parameter list passed into reports/report_template.Rmd,
#' pulling only already-computed results out of the shared session state
#' -- generating a report never triggers new computation.
#' @export
build_report_params <- function(shared, objective) {
  list(
    objective = objective %||% "Not specified",
    file_meta = shared$file_meta,
    qc_metrics = shared$qc_metrics,
    processing_comparison = shared$processing_comparison,
    identification_summary = shared$ident_summary,
    quantification_summary = shared$quant_summary,
    plan_text = if (!is.null(shared$plan)) shared$plan$plan_text else NULL,
    trace_df = provenance_as_dataframe(shared$store),
    warnings = unique(c(shared$qc_metrics$warnings)),
    session_provenance = collect_session_provenance(),
    ai_narrative = shared$final_narrative,
    llm_mode = shared$cfg$llm_mode
  )
}

#' Gather package/session provenance for the report footer.
#' @export
collect_session_provenance <- function() {
  git_sha <- tryCatch({
    out <- suppressWarnings(system("git rev-parse --short HEAD", intern = TRUE, ignore.stderr = TRUE))
    if (length(out) == 0 || !nzchar(out[1])) "unknown" else out[1]
  }, error = function(e) "unknown")

  pkgs <- c("Spectra", "MsExperiment", "MsCoreUtils", "mzR", "ProtGenerics",
            "QFeatures", "SummarizedExperiment", "MultiAssayExperiment", "BiocParallel")
  pkg_versions <- lapply(pkgs, function(p) {
    tryCatch(as.character(utils::packageVersion(p)), error = function(e) "not installed")
  })
  names(pkg_versions) <- pkgs

  list(
    generated_at   = as.character(Sys.time()),
    git_commit_sha = git_sha,
    bioc_version   = get_bioc_version(),
    r_version      = R.version.string,
    package_versions = pkg_versions,
    session_info   = paste(utils::capture.output(utils::sessionInfo()), collapse = "\n")
  )
}
