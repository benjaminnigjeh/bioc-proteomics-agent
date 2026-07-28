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
