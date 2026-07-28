test_that("all expected tools are registered", {
  expected <- c(
    "inspect_uploaded_file", "import_ms_file", "summarize_experiment", "list_spectra", "get_spectrum",
    "calculate_qc_metrics", "plot_tic", "plot_bpc", "plot_spectrum",
    "filter_spectra", "normalize_spectra", "retain_top_peaks", "compare_spectra",
    "import_psm_table", "filter_psms", "summarize_identifications",
    "import_quant_table", "build_qfeatures", "normalize_quantification", "aggregate_to_proteins",
    "run_exploratory_analysis", "generate_report"
  )
  expect_true(all(expected %in% list_tool_names()))
})

test_that("list_tools_for_claude exposes name/description/input_schema only", {
  tools <- list_tools_for_claude()
  expect_true(length(tools) >= 20)
  one <- tools[[1]]
  expect_true(all(c("name", "description", "input_schema") %in% names(one)))
})

test_that("filter_spectra tool creates a new named, retrievable processing stage", {
  ctx <- make_test_ctx()
  sp <- make_test_spectra()
  provenance_put_object(ctx$store, "sp1", sp)

  tool <- get_tool("filter_spectra")
  args <- list(spectra_id = "sp1", stage_name = "ms2_only", ms_level = list(2L))
  res <- guardrail_execute_call(tool, args, ctx)
  expect_true(res$ok)
  expect_equal(res$output_id, "spectra_ms2_only")

  staged <- provenance_get_object(ctx$store, "spectra_ms2_only")
  expect_s4_class(staged, "Spectra")
  expect_true(all(Spectra::msLevel(staged) == 2L))

  # Original object must be untouched.
  original <- provenance_get_object(ctx$store, "sp1")
  expect_equal(length(original), 6)
})

test_that("generate_report tool produces an HTML file via the session context", {
  ctx <- make_test_ctx()
  tool <- get_tool("generate_report")
  res <- guardrail_execute_call(tool, list(objective = "Unit test objective"), ctx)
  expect_true(res$ok)
  expect_true(file.exists(res$output$report_path))
  expect_match(res$output$report_path, "\\.html$")
})
