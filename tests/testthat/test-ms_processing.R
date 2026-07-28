test_that("filter_spectra restricts by MS level", {
  sp <- make_test_spectra()
  ms2_only <- filter_spectra(sp, ms_level = 2L)
  expect_equal(length(ms2_only), 3)
  expect_true(all(Spectra::msLevel(ms2_only) == 2L))
})

test_that("filter_spectra restricts by retention time window", {
  sp <- make_test_spectra()
  windowed <- filter_spectra(sp, rt_min = 1.4, rt_max = 2.6)
  expect_true(all(Spectra::rtime(windowed) >= 1.4 & Spectra::rtime(windowed) <= 2.6))
})

test_that("filter_spectra restricts by precursor m/z range for MS2 scans", {
  sp <- make_test_spectra()
  out <- filter_spectra(sp, precursor_mz_min = 550, precursor_mz_max = 650)
  ms2 <- out[Spectra::msLevel(out) == 2L]
  expect_true(all(Spectra::precursorMz(ms2) >= 550 & Spectra::precursorMz(ms2) <= 650))
})

test_that("filter_spectra restricts by m/z range and minimum intensity", {
  sp <- make_test_spectra()
  out <- filter_spectra(sp, mz_min = 200, mz_max = 400)
  pk <- Spectra::peaksData(out)
  expect_true(all(vapply(pk, function(p) all(p[, "mz"] >= 200 & p[, "mz"] <= 400), logical(1))))

  out2 <- filter_spectra(sp, min_intensity = 500)
  pk2 <- Spectra::peaksData(out2)
  expect_true(all(vapply(pk2, function(p) all(p[, "intensity"] >= 500), logical(1))))
})

test_that("normalize_spectra with method='max' scales the base peak to 1", {
  sp <- make_test_spectra()
  norm <- normalize_spectra(sp, method = "max")
  pk <- Spectra::peaksData(norm)
  non_empty <- Filter(function(p) nrow(p) > 0, pk)
  expect_true(all(vapply(non_empty, function(p) isTRUE(all.equal(max(p[, "intensity"]), 1, tolerance = 1e-6)), logical(1))))
})

test_that("retain_top_peaks keeps at most N peaks per spectrum", {
  sp <- make_test_spectra()
  top2 <- retain_top_peaks(sp, n = 2L)
  counts <- Spectra::lengths(top2)
  expect_true(all(counts <= 2))
})

test_that("compare_spectra reports before/after deltas", {
  sp <- make_test_spectra()
  top1 <- retain_top_peaks(sp, n = 1L)
  cmp <- compare_spectra(sp, top1, "before", "after")
  expect_true(cmp$delta$total_peaks_delta <= 0)
  expect_named(cmp, c("before", "after", "delta"))
})
