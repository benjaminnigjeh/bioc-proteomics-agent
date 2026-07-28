test_that("provenance store records entries with all expected fields", {
  store <- new_provenance_store()
  provenance_add_entry(store, agent = "qc", objective = "test objective", plan_step = 1,
                        tool = "calculate_qc_metrics", reason = "unit test", arguments = list(spectra_id = "sp1"),
                        r_function = "calculate_qc_metrics", input_id = "sp1", output_id = NA_character_,
                        status = "ok", duration_s = 0.1, warnings = c("MEDIUM: something"))
  df <- provenance_as_dataframe(store)
  expect_equal(nrow(df), 1)
  expect_equal(df$agent, "qc")
  expect_equal(df$tool, "calculate_qc_metrics")
  expect_equal(df$status, "ok")
  expect_match(df$warnings, "something")
})

test_that("provenance object store round-trips arbitrary R objects by id", {
  store <- new_provenance_store()
  sp <- make_test_spectra()
  provenance_put_object(store, "spectra_1", sp)
  fetched <- provenance_get_object(store, "spectra_1")
  expect_identical(fetched, sp)
  expect_null(provenance_get_object(store, "does_not_exist"))
})

test_that("provenance_as_list produces a plain list safe for jsonlite", {
  store <- new_provenance_store()
  provenance_add_entry(store, agent = "reporting", objective = "test", plan_step = 1,
                        tool = "generate_report", reason = "unit test", arguments = list(objective = "test"),
                        r_function = "generate_report", status = "ok")
  lst <- provenance_as_list(store)
  expect_length(lst, 1)
  json <- jsonlite::toJSON(lst, auto_unbox = TRUE, null = "null")
  expect_true(nzchar(json))
})

test_that("redact_secrets removes API-key-shaped strings", {
  cfg <- list(anthropic_api_key = "sk-ant-secretvalue1234567890")
  text <- "Authorization failed for key sk-ant-secretvalue1234567890 during request"
  redacted <- redact_secrets(text, cfg)
  expect_false(grepl("secretvalue", redacted, fixed = TRUE))
  expect_match(redacted, "REDACTED")
})
