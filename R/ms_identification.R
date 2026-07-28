# R/ms_identification.R
#
# Deterministic PSM identification-table handling for the Identification
# Agent. Works on plain data.frames (imported CSV/TSV); when possible,
# identifications are linked back to a Spectra object by scan/spectrum id.

REQUIRED_PSM_FIELDS <- c("peptide", "protein")
RECOMMENDED_PSM_FIELDS <- c("scan", "charge", "score", "qvalue")

#' Detect delimiter and import a PSM table from a CSV/TSV/TXT path.
#' @export
import_psm_table <- function(path) {
  if (!requireNamespace("utils", quietly = TRUE)) stop("utils required.")
  first_line <- readLines(path, n = 1, warn = FALSE)
  delim <- if (lengths(regmatches(first_line, gregexpr("\t", first_line))) >
               lengths(regmatches(first_line, gregexpr(",", first_line)))) "\t" else ","
  df <- utils::read.delim(path, sep = delim, header = TRUE, stringsAsFactors = FALSE,
                           check.names = FALSE)
  attr(df, "detected_delimiter") <- if (delim == "\t") "tab" else "comma"
  df
}

#' Validate a PSM table has the minimum required columns (after best-effort
#' case-insensitive column mapping). Returns validation result + a mapped
#' data.frame with canonical lowercase column names where recognized.
#'
#' @export
validate_psm_table <- function(df, column_map = NULL) {
  errors <- character(0)
  names_lower <- tolower(trimws(colnames(df)))
  colnames(df) <- names_lower

  # Best-effort automatic mapping of common PSM export column names.
  synonyms <- list(
    peptide = c("peptide", "sequence", "peptide_sequence", "peptideseq"),
    protein = c("protein", "protein_id", "proteins", "accession", "master_protein"),
    scan    = c("scan", "scannumber", "scan_number", "spectrum", "spectrum_id"),
    charge  = c("charge", "precursor_charge", "z"),
    score   = c("score", "xcorr", "hyperscore", "search_score"),
    qvalue  = c("qvalue", "q_value", "fdr", "percolator_qvalue")
  )
  canonical <- list()
  for (field in names(synonyms)) {
    hit <- intersect(synonyms[[field]], names_lower)
    if (length(hit) > 0) canonical[[field]] <- hit[1]
  }
  if (!is.null(column_map)) {
    for (field in names(column_map)) {
      if (column_map[[field]] %in% names_lower) canonical[[field]] <- tolower(column_map[[field]])
    }
  }

  missing_required <- setdiff(REQUIRED_PSM_FIELDS, names(canonical))
  if (length(missing_required) > 0) {
    errors <- c(errors, sprintf("Missing required PSM field(s): %s.", paste(missing_required, collapse = ", ")))
  }

  ok <- length(errors) == 0
  mapped_df <- NULL
  if (ok) {
    mapped_df <- df
    rename_idx <- match(unlist(canonical), colnames(mapped_df))
    colnames(mapped_df)[rename_idx] <- names(canonical)
  }

  list(ok = ok, errors = errors, column_map = canonical, n_rows = nrow(df), mapped_df = mapped_df)
}

#' Filter a validated PSM data.frame by score and/or q-value/FDR threshold.
#' @export
filter_psms <- function(df, min_score = NULL, max_qvalue = NULL) {
  out <- df
  if (!is.null(min_score) && "score" %in% colnames(out)) {
    out <- out[!is.na(out$score) & out$score >= min_score, , drop = FALSE]
  }
  if (!is.null(max_qvalue) && "qvalue" %in% colnames(out)) {
    out <- out[!is.na(out$qvalue) & out$qvalue <= max_qvalue, , drop = FALSE]
  }
  out
}

#' Attempt to link PSMs to spectra in a Spectra object by scan number,
#' matching against the object's acquisitionNum / spectrumId if available.
#' Returns the PSM data.frame with an added `linked` logical column.
#' @export
link_psms_to_spectra <- function(df, sp) {
  if (!"scan" %in% colnames(df) || is.null(sp)) {
    df$linked <- FALSE
    return(df)
  }
  scan_ids <- tryCatch(as.character(Spectra::acquisitionNum(sp)), error = function(e) character(0))
  df$linked <- as.character(df$scan) %in% scan_ids
  df
}

#' Summarize PSMs -> peptides -> proteins from a (filtered) PSM data.frame.
#' @export
summarize_identifications <- function(df) {
  n_psm <- nrow(df)
  n_peptide <- if ("peptide" %in% colnames(df)) length(unique(df$peptide)) else NA_integer_
  n_protein <- if ("protein" %in% colnames(df)) length(unique(df$protein)) else NA_integer_

  peptide_summary <- if ("peptide" %in% colnames(df)) {
    as.data.frame(table(peptide = df$peptide), stringsAsFactors = FALSE)
  } else data.frame()
  protein_summary <- if ("protein" %in% colnames(df)) {
    as.data.frame(table(protein = df$protein), stringsAsFactors = FALSE)
  } else data.frame()

  list(
    n_psm = n_psm,
    n_unique_peptides = n_peptide,
    n_unique_proteins = n_protein,
    median_psms_per_protein = if (nrow(protein_summary) > 0) stats::median(protein_summary$Freq) else NA_real_,
    peptide_summary = peptide_summary,
    protein_summary = protein_summary
  )
}
