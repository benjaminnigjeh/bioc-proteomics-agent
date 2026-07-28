test_that("mock_generate_plan builds a spectra-only plan when only spectra are available", {
  available <- list(spectra_id = "sp1", has_psm = FALSE, has_quant = FALSE)
  plan <- mock_generate_plan("Inspect and QC this run.", available, list(max_agent_steps = 12))
  tool_names <- vapply(plan$steps, function(s) s$tool, character(1))
  expect_true("calculate_qc_metrics" %in% tool_names)
  expect_true("filter_spectra" %in% tool_names)
  expect_equal(tool_names[length(tool_names)], "generate_report")
  expect_equal(plan$source, "mock")
})

test_that("mock_generate_plan adds identification and quantification steps when available", {
  available <- list(spectra_id = "sp1", has_psm = TRUE, psm_id = "psm1",
                     has_quant = TRUE, quant_table_id = "qt1", quant_id_col = "peptide",
                     quant_sample_cols = c("s1", "s2"))
  plan <- mock_generate_plan("Full analysis.", available, list(max_agent_steps = 12))
  tool_names <- vapply(plan$steps, function(s) s$tool, character(1))
  expect_true("summarize_identifications" %in% tool_names)
  expect_true("build_qfeatures" %in% tool_names)
})

test_that("mock_next_action walks the plan in order and terminates with a final summary", {
  available <- list(spectra_id = "sp1", has_psm = FALSE, has_quant = FALSE)
  plan <- mock_generate_plan("Inspect this run.", available, list(max_agent_steps = 12))
  state <- list(cursor = 1L, outputs = list(), results = list(), plan = plan)

  action1 <- mock_next_action(state)
  expect_equal(action1$type, "tool_use")
  expect_equal(action1$tool, plan$steps[[1]]$tool)

  state$cursor <- length(plan$steps) + 1L
  final <- mock_next_action(state)
  expect_equal(final$type, "final")
  expect_match(final$text, "MOCK LLM")
})
