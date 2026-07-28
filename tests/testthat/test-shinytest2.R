# tests/testthat/test-shinytest2.R
#
# Small end-to-end smoke test driving a real (headless) browser session
# against the app: load demo data, run QC, create a mock plan, execute it,
# and generate a report. Always runs with LLM_MODE=mock -- the real
# Claude API is never called from automated tests.
#
# Gracefully skipped (not failed) if no headless Chrome/Chromium is
# available in the current environment.

test_that("end-to-end: demo data -> QC -> mock plan -> execute -> report", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("withr")

  withr::local_envvar(c(LLM_MODE = "mock"))
  app_dir <- normalizePath(.test_root)

  app <- tryCatch(
    shinytest2::AppDriver$new(app_dir = app_dir, name = "bpa-e2e",
                               timeout = 30000, load_timeout = 90000, height = 1000, width = 1400),
    error = function(e) NULL
  )
  testthat::skip_if(is.null(app), "No headless Chrome/Chromium session available for shinytest2.")
  withr::defer(app$stop())

  app$click(selector = "#home-load_demo")
  app$wait_for_idle(timeout = 20000)

  app$click(selector = "a[data-value='Quality Control']")
  app$click(selector = "#qc-run_qc")
  app$wait_for_idle(timeout = 20000)
  qc_summary <- app$get_text("#qc-qc_summary")
  expect_match(qc_summary, "Total spectra")

  app$click(selector = "a[data-value='Agent Workspace']")
  app$set_inputs(`agent-objective` = "Inspect this run, evaluate QC, and prepare a reproducible report.")
  app$click(selector = "#agent-create_plan")
  app$wait_for_idle(timeout = 15000)
  plan_text <- app$get_text("#agent-plan_ui")
  expect_match(plan_text, "Plan for objective")

  app$click(selector = "#agent-run_plan")
  app$wait_for_idle(timeout = 30000)
  status_text <- app$get_text("#agent-agent_status")
  expect_match(status_text, "complete")

  app$click(selector = "a[data-value='Report']")
  app$click(selector = "#report-generate")
  app$wait_for_idle(timeout = 30000)
  report_status <- app$get_text("#report-status")
  expect_match(report_status, "successfully")
})
