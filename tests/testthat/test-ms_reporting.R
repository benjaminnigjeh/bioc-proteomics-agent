test_that("export_qc_table writes a readable CSV with expected metrics", {
  sp <- make_test_spectra()
  qc <- calculate_qc_metrics(sp)
  dir <- tempfile("bpa_report_"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  path <- export_qc_table(qc, dir)
  expect_true(file.exists(path))
  df <- utils::read.csv(path)
  expect_true("n_spectra" %in% df$metric)
})

test_that("export_provenance_json writes valid JSON reflecting the trace", {
  store <- new_provenance_store()
  provenance_add_entry(store, agent = "qc", objective = "obj", plan_step = 1, tool = "calculate_qc_metrics",
                        reason = "r", arguments = list(), r_function = "calculate_qc_metrics", status = "ok")
  dir <- tempfile("bpa_report_"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  path <- export_provenance_json(store, dir)
  expect_true(file.exists(path))
  parsed <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_length(parsed, 1)
  expect_equal(parsed[[1]]$tool, "calculate_qc_metrics")
})

test_that("export_trace_csv writes the full agent trace", {
  store <- new_provenance_store()
  provenance_add_entry(store, agent = "data_intake", objective = "obj", plan_step = 1, tool = "import_ms_file",
                        reason = "r", arguments = list(), r_function = "import_ms_file", status = "ok")
  dir <- tempfile("bpa_report_"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  path <- export_trace_csv(store, dir)
  df <- utils::read.csv(path)
  expect_equal(nrow(df), 1)
  expect_equal(df$tool, "import_ms_file")
})

test_that("collect_session_provenance returns package version and R version info", {
  sp_info <- collect_session_provenance()
  expect_true("Spectra" %in% names(sp_info$package_versions))
  expect_true(nzchar(sp_info$r_version))
  expect_true(nzchar(sp_info$generated_at))
})

test_that("report template parameters build without error and contain required sections", {
  shared_stub <- list(
    file_meta = list(original_filename = "demo.mzML"),
    qc_metrics = calculate_qc_metrics(make_test_spectra()),
    processing_comparison = NULL,
    ident_summary = NULL,
    quant_summary = NULL,
    plan = list(plan_text = "1. do a thing"),
    store = new_provenance_store(),
    final_narrative = "test narrative",
    cfg = list(llm_mode = "mock")
  )
  params <- build_report_params(shared_stub, "Test objective")
  expect_equal(params$objective, "Test objective")
  expect_false(is.null(params$qc_metrics))
  expect_equal(params$llm_mode, "mock")
})
