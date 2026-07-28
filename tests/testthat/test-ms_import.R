test_that("a synthetic Spectra object has the expected structure", {
  sp <- make_test_spectra()
  expect_s4_class(sp, "Spectra")
  expect_equal(length(sp), 6)
  expect_equal(sort(unique(Spectra::msLevel(sp))), c(1L, 2L))
})

test_that("build_ms_experiment wraps a Spectra object into an MsExperiment", {
  sp <- make_test_spectra()
  ms_exp <- build_ms_experiment(sp, "unit_test_sample")
  expect_s4_class(ms_exp, "MsExperiment")
  expect_equal(length(MsExperiment::spectra(ms_exp)), length(sp))
})

test_that("summarize_experiment reports correct counts", {
  sp <- make_test_spectra()
  s <- summarize_experiment(sp)
  expect_equal(s$n_spectra, 6)
  expect_equal(s$ms_level_counts$ms1, 3)
  expect_equal(s$ms_level_counts$ms2, 3)
})

test_that("import_ms_file reads the bundled demo mzML when present", {
  demo_path <- file.path(.test_root, "inst", "extdata", "demo_lcmsms.mzML")
  testthat::skip_if_not(file.exists(demo_path), "Demo mzML not generated (run scripts/generate_demo_data.R).")
  result <- import_ms_file(demo_path, "demo_lcmsms.mzML")
  expect_s4_class(result$spectra, "Spectra")
  expect_s4_class(result$ms_experiment, "MsExperiment")
  expect_true(length(result$spectra) > 0)
  expect_true(all(result$meta$ms_levels %in% c(1L, 2L)))
})

test_that("inspect_uploaded_file returns checksum and detected format", {
  f <- tempfile(fileext = ".mzML")
  writeLines("not real mzml content", f)
  on.exit(unlink(f))
  info <- inspect_uploaded_file(f, "demo.mzML")
  expect_equal(info$detected_format, "mzML")
  expect_equal(nchar(info$sha256), 64)
})
