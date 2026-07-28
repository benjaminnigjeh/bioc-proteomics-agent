# R/ms_processing.R
#
# Deterministic spectrum processing for the Spectrum Processing Agent:
# filtering, normalization, top-N peak retention, and before/after
# comparison. Every function returns a *new* Spectra object -- the
# original is never mutated in place, so processing stages can be
# named and chained while preserving provenance.

#' Filter spectra by MS level, retention time, precursor m/z, m/z range,
#' and/or intensity thresholds. Any argument left NULL is not applied.
#'
#' @export
filter_spectra <- function(sp,
                            ms_level = NULL,
                            rt_min = NULL, rt_max = NULL,
                            precursor_mz_min = NULL, precursor_mz_max = NULL,
                            mz_min = NULL, mz_max = NULL,
                            min_intensity = NULL,
                            min_relative_intensity = NULL) {
  if (!requireNamespace("Spectra", quietly = TRUE)) stop("Spectra package required.")
  out <- sp

  if (!is.null(ms_level)) {
    out <- out[Spectra::msLevel(out) %in% ms_level]
  }
  if (!is.null(rt_min) || !is.null(rt_max)) {
    rt <- Spectra::rtime(out)
    lo <- if (is.null(rt_min)) -Inf else rt_min
    hi <- if (is.null(rt_max)) Inf else rt_max
    out <- out[!is.na(rt) & rt >= lo & rt <= hi]
  }
  if (!is.null(precursor_mz_min) || !is.null(precursor_mz_max)) {
    pmz <- tryCatch(Spectra::precursorMz(out), error = function(e) rep(NA_real_, length(out)))
    lo <- if (is.null(precursor_mz_min)) -Inf else precursor_mz_min
    hi <- if (is.null(precursor_mz_max)) Inf else precursor_mz_max
    keep <- Spectra::msLevel(out) == 1 | (!is.na(pmz) & pmz >= lo & pmz <= hi)
    out <- out[keep]
  }
  if (!is.null(mz_min) || !is.null(mz_max)) {
    lo <- if (is.null(mz_min)) 0 else mz_min
    hi <- if (is.null(mz_max)) Inf else mz_max
    out <- Spectra::filterMzRange(out, mz = c(lo, hi))
  }
  if (!is.null(min_intensity)) {
    out <- Spectra::filterIntensity(out, intensity = c(min_intensity, Inf))
  }
  if (!is.null(min_relative_intensity)) {
    rel_fun <- function(x, spectrumMsLevel) {
      thr <- min_relative_intensity * max(x, na.rm = TRUE)
      x >= thr
    }
    out <- Spectra::filterIntensity(out, intensity = rel_fun)
  }
  out
}

#' Normalize spectrum intensities. method = "tic" (divide by total ion
#' current) or "max" (divide by base peak intensity).
#' @export
normalize_spectra <- function(sp, method = c("tic", "max")) {
  method <- match.arg(method)
  if (!requireNamespace("Spectra", quietly = TRUE)) stop("Spectra package required.")
  Spectra::addProcessing(sp, function(x, ...) {
    if (nrow(x) == 0) return(x)
    denom <- if (method == "tic") sum(x[, "intensity"], na.rm = TRUE) else max(x[, "intensity"], na.rm = TRUE)
    if (is.na(denom) || denom == 0) return(x)
    x[, "intensity"] <- x[, "intensity"] / denom
    x
  })
}

#' Retain only the top-N most intense peaks per spectrum.
#' @export
retain_top_peaks <- function(sp, n = 50L) {
  if (!requireNamespace("Spectra", quietly = TRUE)) stop("Spectra package required.")
  if (!requireNamespace("MsCoreUtils", quietly = TRUE)) stop("MsCoreUtils package required.")
  Spectra::filterIntensity(sp, intensity = function(x, spectrumMsLevel) {
    if (length(x) <= n) return(rep(TRUE, length(x)))
    thr <- sort(x, decreasing = TRUE)[n]
    x >= thr
  })
}

#' Compare aggregate metrics of two Spectra objects (e.g. before vs after
#' processing). Returns a compact before/after table as a list.
#' @export
compare_spectra <- function(sp_before, sp_after, label_before = "before", label_after = "after") {
  summarize_one <- function(sp) {
    n <- length(sp)
    peaks <- Spectra::lengths(sp)
    list(
      n_spectra = n,
      mean_peaks_per_spectrum = if (n > 0) mean(peaks, na.rm = TRUE) else NA_real_,
      median_peaks_per_spectrum = if (n > 0) stats::median(peaks, na.rm = TRUE) else NA_real_,
      total_peaks = if (n > 0) sum(peaks, na.rm = TRUE) else 0L,
      mean_tic = if (n > 0) mean(Spectra::tic(sp), na.rm = TRUE) else NA_real_
    )
  }
  b <- summarize_one(sp_before)
  a <- summarize_one(sp_after)
  list(
    before = setNames(b, paste0(label_before, "_", names(b))),
    after  = setNames(a, paste0(label_after, "_", names(a))),
    delta  = list(
      n_spectra_delta = a$n_spectra - b$n_spectra,
      mean_peaks_per_spectrum_delta = a$mean_peaks_per_spectrum - b$mean_peaks_per_spectrum,
      total_peaks_delta = a$total_peaks - b$total_peaks,
      total_peaks_pct_change = if (b$total_peaks > 0) 100 * (a$total_peaks - b$total_peaks) / b$total_peaks else NA_real_
    )
  )
}
