# R/ms_qc.R
#
# Deterministic QC metric calculation and plotting for the QC Agent.
# All functions here operate on a Spectra object and return plain lists /
# data.frames / ggplot objects -- never narrative text. Narrative
# interpretation is produced later by Claude, grounded in these numbers.

#' Base peak intensity per spectrum, computed directly from peak data
#' (the maximum intensity value in each spectrum), rather than relying on
#' any single "ionCount"-style accessor whose exact semantics vary.
.base_peak_intensity <- function(sp) {
  int_list <- Spectra::intensity(sp)
  vapply(int_list, function(ints) {
    if (length(ints) == 0) return(NA_real_)
    max(ints, na.rm = TRUE)
  }, numeric(1))
}

#' Calculate file- and spectrum-level QC metrics for a Spectra object.
#'
#' @param sp a Spectra object
#' @return a list of QC metrics plus a `warnings` character vector with
#'   severity-tagged messages (e.g. "HIGH: ...", "MEDIUM: ...", "LOW: ...")
#' @export
calculate_qc_metrics <- function(sp) {
  if (!requireNamespace("Spectra", quietly = TRUE)) stop("Spectra package required.")
  if (!requireNamespace("MsCoreUtils", quietly = TRUE)) {
    warning("MsCoreUtils not available; some metrics may be skipped.")
  }

  n <- length(sp)
  warnings_out <- character(0)

  if (n == 0) {
    return(list(
      n_spectra = 0, n_ms1 = 0, n_ms2 = 0, ms2_ms1_ratio = NA_real_,
      rt_range_s = c(NA_real_, NA_real_), peaks_per_spectrum = list(),
      tic_summary = list(), bpi_summary = list(), precursor_mz_summary = list(),
      charge_counts = list(), n_missing_precursor = NA_integer_,
      n_empty_spectra = 0, warnings = c("HIGH: No spectra found in file.")
    ))
  }

  ms_level <- Spectra::msLevel(sp)
  n_ms1 <- sum(ms_level == 1, na.rm = TRUE)
  n_ms2 <- sum(ms_level == 2, na.rm = TRUE)
  ratio <- if (n_ms1 > 0) n_ms2 / n_ms1 else NA_real_

  rt <- Spectra::rtime(sp)
  peaks_count <- lengths(sp)
  tic <- Spectra::tic(sp, initial = FALSE)
  bpi <- tryCatch(.base_peak_intensity(sp), error = function(e) rep(NA_real_, n))

  precursor_mz <- tryCatch(Spectra::precursorMz(sp), error = function(e) rep(NA_real_, n))
  precursor_charge <- tryCatch(Spectra::precursorCharge(sp), error = function(e) rep(NA_integer_, n))

  n_empty <- sum(peaks_count == 0, na.rm = TRUE)
  n_missing_precursor <- sum(ms_level == 2 & is.na(precursor_mz), na.rm = TRUE)

  if (n_empty > 0) {
    warnings_out <- c(warnings_out, sprintf("MEDIUM: %d empty spectra detected (0 peaks).", n_empty))
  }
  if (n_ms1 == 0) {
    warnings_out <- c(warnings_out, "HIGH: No MS1 spectra found; precursor context may be unavailable.")
  }
  if (n_ms2 == 0) {
    warnings_out <- c(warnings_out, "MEDIUM: No MS2 spectra found; MS/MS-dependent steps will be skipped.")
  }
  if (!is.na(n_missing_precursor) && n_missing_precursor > 0) {
    warnings_out <- c(warnings_out, sprintf("MEDIUM: %d MS2 spectra missing precursor m/z.", n_missing_precursor))
  }
  if (!is.na(ratio) && ratio < 0.5 && n_ms2 > 0) {
    warnings_out <- c(warnings_out, "LOW: MS2/MS1 ratio is unusually low for a typical DDA run.")
  }

  charge_tab <- table(precursor_charge[ms_level == 2], useNA = "ifany")

  list(
    n_spectra            = n,
    n_ms1                = n_ms1,
    n_ms2                = n_ms2,
    ms2_ms1_ratio         = ratio,
    rt_range_s            = as.numeric(range(rt, na.rm = TRUE)),
    peaks_per_spectrum    = list(
      min = as.numeric(min(peaks_count, na.rm = TRUE)),
      median = as.numeric(stats::median(peaks_count, na.rm = TRUE)),
      max = as.numeric(max(peaks_count, na.rm = TRUE)),
      mean = as.numeric(mean(peaks_count, na.rm = TRUE))
    ),
    tic_summary           = list(
      min = as.numeric(min(tic, na.rm = TRUE)),
      median = as.numeric(stats::median(tic, na.rm = TRUE)),
      max = as.numeric(max(tic, na.rm = TRUE))
    ),
    bpi_summary            = list(
      min = suppressWarnings(as.numeric(min(bpi, na.rm = TRUE))),
      median = suppressWarnings(as.numeric(stats::median(bpi, na.rm = TRUE))),
      max = suppressWarnings(as.numeric(max(bpi, na.rm = TRUE)))
    ),
    precursor_mz_summary   = if (n_ms2 > 0) list(
      min = as.numeric(min(precursor_mz[ms_level == 2], na.rm = TRUE)),
      median = as.numeric(stats::median(precursor_mz[ms_level == 2], na.rm = TRUE)),
      max = as.numeric(max(precursor_mz[ms_level == 2], na.rm = TRUE))
    ) else list(),
    charge_counts          = as.list(setNames(as.integer(charge_tab), paste0("z", names(charge_tab)))),
    n_missing_precursor     = as.integer(n_missing_precursor),
    n_empty_spectra         = as.integer(n_empty),
    warnings               = warnings_out
  )
}

#' Total ion chromatogram plot (ggplot2), MS1 by default.
#' @export
plot_tic <- function(sp, ms_level = 1L) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required for plotting.")
  sub <- sp[Spectra::msLevel(sp) == ms_level]
  df <- data.frame(rtime = Spectra::rtime(sub), tic = Spectra::tic(sub, initial = FALSE))
  df <- df[order(df$rtime), , drop = FALSE]
  ggplot2::ggplot(df, ggplot2::aes(x = .data$rtime, y = .data$tic)) +
    ggplot2::geom_line(color = "#2C6E9A") +
    ggplot2::labs(title = sprintf("Total Ion Chromatogram (MS%d)", ms_level),
                  x = "Retention time (s)", y = "Total ion current") +
    ggplot2::theme_minimal()
}

#' Base peak chromatogram plot (ggplot2), MS1 by default.
#' @export
plot_bpc <- function(sp, ms_level = 1L) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required for plotting.")
  sub <- sp[Spectra::msLevel(sp) == ms_level]
  bpi <- tryCatch(.base_peak_intensity(sub), error = function(e) rep(NA_real_, length(sub)))
  df <- data.frame(rtime = Spectra::rtime(sub), bpi = bpi)
  df <- df[order(df$rtime), , drop = FALSE]
  ggplot2::ggplot(df, ggplot2::aes(x = .data$rtime, y = .data$bpi)) +
    ggplot2::geom_line(color = "#B0562B") +
    ggplot2::labs(title = sprintf("Base Peak Chromatogram (MS%d)", ms_level),
                  x = "Retention time (s)", y = "Base peak intensity") +
    ggplot2::theme_minimal()
}

#' Stick plot of a single spectrum (m/z vs intensity).
#' @export
plot_spectrum <- function(sp, index) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required for plotting.")
  if (index < 1 || index > length(sp)) stop("Spectrum index out of range.")
  one <- sp[index]
  pk <- Spectra::peaksData(one)[[1]]
  df <- as.data.frame(pk)
  colnames(df) <- c("mz", "intensity")
  ms_lvl <- Spectra::msLevel(one)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$mz, xend = .data$mz, y = 0, yend = .data$intensity)) +
    ggplot2::geom_segment(color = "#3B3B3B") +
    ggplot2::labs(title = sprintf("Spectrum #%d (MS%d)", index, ms_lvl),
                  x = "m/z", y = "Intensity") +
    ggplot2::theme_minimal()
}
