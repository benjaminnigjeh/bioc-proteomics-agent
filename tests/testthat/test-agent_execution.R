.make_test_cfg <- function(max_agent_steps = 12) {
  list(anthropic_api_key = "", anthropic_model = "claude-sonnet-4-5", llm_mode = "mock",
       max_agent_steps = max_agent_steps, max_tool_retries = 3, claude_request_timeout = 30,
       shiny_port = 3838, max_upload_mb = 500)
}

test_that("a full mock multi-agent run completes and produces a grounded trace + report", {
  ctx <- make_test_ctx()
  provenance_put_object(ctx$store, "sp1", make_test_spectra())
  available <- list(spectra_id = "sp1", has_psm = FALSE, has_quant = FALSE)
  cfg <- .make_test_cfg()
  objective <- "Inspect this run, evaluate QC, apply a conservative MS2 filter, and report."

  plan <- agent_create_plan(objective, "mock", cfg, available, AGENT_DESCRIPTIONS)
  expect_true(length(plan$steps) > 0)

  result <- agent_execute_plan(plan, objective, "mock", cfg, ctx, AGENT_DESCRIPTIONS)

  expect_equal(result$status, "complete")
  expect_true(length(result$trace) == length(plan$steps))
  expect_true(all(vapply(result$trace, function(e) e$status, character(1)) == "ok"))
  expect_match(result$final_text, "MOCK LLM")

  tools_called <- vapply(result$trace, function(e) e$tool, character(1))
  expect_true("calculate_qc_metrics" %in% tools_called)
  expect_true("generate_report" %in% tools_called)
})

test_that("the agent loop stops at the configured maximum step count", {
  ctx <- make_test_ctx()
  provenance_put_object(ctx$store, "sp1", make_test_spectra())
  available <- list(spectra_id = "sp1", has_psm = FALSE, has_quant = FALSE)
  cfg <- .make_test_cfg(max_agent_steps = 2)
  objective <- "Inspect this run, evaluate QC, apply a conservative MS2 filter, and report."

  plan <- agent_create_plan(objective, "mock", cfg, available, AGENT_DESCRIPTIONS)
  expect_true(length(plan$steps) > 2) # plan is longer than the step budget

  result <- agent_execute_plan(plan, objective, "mock", cfg, ctx, AGENT_DESCRIPTIONS)

  expect_equal(result$status, "max_steps_reached")
  expect_equal(length(result$trace), 2)
  expect_true(any(grepl("maximum of 2 agent steps", result$warnings)))
})

test_that("a plan-generation error (e.g. Claude misconfigured) is surfaced without executing tools", {
  ctx <- make_test_ctx()
  cfg <- .make_test_cfg()
  cfg$llm_mode <- "claude"
  cfg$anthropic_api_key <- "" # not configured -> claude_generate_plan must fail cleanly
  available <- list(spectra_id = NULL, has_psm = FALSE, has_quant = FALSE)

  plan <- agent_create_plan("Do something.", "claude", cfg, available, AGENT_DESCRIPTIONS)
  expect_true(!is.null(plan$error))
  expect_equal(plan$error$type, "not_configured")

  result <- agent_execute_plan(plan, "Do something.", "claude", cfg, ctx, AGENT_DESCRIPTIONS)
  expect_equal(result$status, "error")
  expect_equal(length(result$trace), 0)
})
