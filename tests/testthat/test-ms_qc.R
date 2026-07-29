test_that("calculate_qc_metrics computes expected counts and ratio", {
  sp <- make_test_spectra()
  qc <- calculate_qc_metrics(sp)
  expect_equal(qc$n_spectra, 6)
  expect_equal(qc$n_ms1, 3)
  expect_equal(qc$n_ms2, 3)
  expect_equal(qc$ms2_ms1_ratio, 1)
  expect_equal(qc$n_empty_spectra, 1) # one MS1 scan has a single zero-length peak marker
})

test_that("calculate_qc_metrics flags empty spectra with a warning", {
  sp <- make_test_spectra()
  qc <- calculate_qc_metrics(sp)
  expect_true(any(grepl("empty spectra", qc$warnings, ignore.case = TRUE)))
})

test_that("calculate_qc_metrics handles an empty Spectra object without erroring", {
  sp <- make_test_spectra()
  empty_sp <- sp[integer(0)]
  qc <- calculate_qc_metrics(empty_sp)
  expect_equal(qc$n_spectra, 0)
  expect_true(any(grepl("HIGH", qc$warnings)))
})

test_that("calculate_msquality_metrics returns a non-empty named list of standardized metrics", {
  skip_if_not_installed("MsQuality")
  sp <- make_test_spectra()
  m <- calculate_msquality_metrics(sp)
  expect_true(is.list(m))
  expect_true(length(m) > 0)
  expect_true(!is.null(names(m)) && all(nzchar(names(m))))
  expect_true("numberSpectra" %in% names(m))
  expect_true(is.numeric(m[["numberSpectra"]]))
})
