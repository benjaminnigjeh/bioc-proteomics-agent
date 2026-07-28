# R/ms_import.R
#
# Deterministic Bioconductor import functions used by the Data Intake Agent.
# Wraps Spectra / MsExperiment construction. No Claude call ever reaches
# this file directly -- it is only invoked through the tool registry.

#' Inspect an uploaded file without fully parsing it: extension, size,
#' checksum, and a best-effort format guess.
#'
#' @export
inspect_uploaded_file <- function(path, original_filename) {
  if (!file.exists(path)) stop("File does not exist: ", path)
  ext <- file_extension(original_filename)
  info <- file.info(path)
  list(
    original_filename = original_filename,
    extension         = ext,
    size_bytes        = as.numeric(info$size),
    sha256            = file_checksum(path),
    detected_format   = switch(ext,
                                mzml  = "mzML",
                                mzxml = "mzXML",
                                mgf   = "MGF",
                                "unknown"),
    modified_time     = as.character(info$mtime)
  )
}

#' Import a spectral data file into a Spectra object using the appropriate
#' Bioconductor backend, then wrap it into an MsExperiment.
#'
#' @param path local, already-validated file path
#' @param original_filename original name for metadata/display purposes
#' @return list(spectra = Spectra, ms_experiment = MsExperiment, meta = list)
#' @export
import_ms_file <- function(path, original_filename) {
  if (!requireNamespace("Spectra", quietly = TRUE)) {
    stop("The 'Spectra' package is required to import MS data.")
  }
  ext <- file_extension(original_filename)

  sp <- if (ext %in% c("mzml", "mzxml")) {
    # MsBackendMzR is a backend class exported directly by the Spectra
    # package itself (not a separate installable package).
    Spectra::Spectra(path, source = Spectra::MsBackendMzR())
  } else if (ext == "mgf") {
    if (!requireNamespace("MsBackendMgf", quietly = TRUE)) {
      stop("The 'MsBackendMgf' package is required to import MGF files.")
    }
    Spectra::Spectra(path, source = MsBackendMgf::MsBackendMgf())
  } else {
    stop("Unsupported file extension for MS import: ", ext)
  }

  ms_exp <- build_ms_experiment(sp, original_filename)

  meta <- list(
    n_spectra    = length(sp),
    ms_levels    = sort(unique(Spectra::msLevel(sp))),
    rt_range_s   = if (length(sp) > 0) range(Spectra::rtime(sp), na.rm = TRUE) else c(NA_real_, NA_real_),
    source_file  = original_filename,
    format       = toupper(ext)
  )

  list(spectra = sp, ms_experiment = ms_exp, meta = meta)
}

#' Build an MsExperiment container around a Spectra object.
#' @export
build_ms_experiment <- function(sp, sample_name) {
  if (!requireNamespace("MsExperiment", quietly = TRUE)) {
    stop("The 'MsExperiment' package is required.")
  }
  sample_df <- S4Vectors::DataFrame(sample_id = sample_name, sample_name = sample_name)
  ms_exp <- MsExperiment::MsExperiment(sampleData = sample_df)
  MsExperiment::spectra(ms_exp) <- sp
  ms_exp
}

#' Produce a compact, display-friendly summary of an MsExperiment / Spectra
#' pair. Used both by the UI "Upload and Experiment" panel and the
#' `summarize_experiment` tool.
#'
#' @export
summarize_experiment <- function(sp, ms_exp = NULL) {
  if (!requireNamespace("Spectra", quietly = TRUE)) {
    stop("The 'Spectra' package is required.")
  }
  n <- length(sp)
  levels_tab <- table(Spectra::msLevel(sp))
  rt <- if (n > 0) Spectra::rtime(sp) else numeric(0)

  list(
    n_spectra       = n,
    ms_level_counts = as.list(setNames(as.integer(levels_tab), paste0("ms", names(levels_tab)))),
    rt_range_s      = if (n > 0) as.numeric(range(rt, na.rm = TRUE)) else c(NA_real_, NA_real_),
    polarity_counts = tryCatch(as.list(table(Spectra::polarity(sp))), error = function(e) list()),
    has_ms_experiment = !is.null(ms_exp)
  )
}
