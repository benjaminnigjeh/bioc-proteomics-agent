test_that("assert_registry_is_safe finds no forbidden tool names", {
  expect_true(assert_registry_is_safe())
})

test_that("unknown tools are rejected by the guardrail", {
  ctx <- make_test_ctx()
  res <- guardrail_validate_call("eval_arbitrary_r_code", list(), ctx)
  expect_false(res$ok)
  expect_match(res$errors, "not registered")
})

test_that("schema_check rejects missing required arguments", {
  schema <- list(type = "object",
                  properties = list(spectra_id = list(type = "string")),
                  required = list("spectra_id"))
  res <- schema_check(list(), schema)
  expect_false(res$ok)
  expect_match(res$errors, "Missing required")
})

test_that("schema_check rejects unknown arguments not declared in the schema", {
  schema <- list(type = "object", properties = list(a = list(type = "string")), required = list())
  res <- schema_check(list(a = "x", b = "y"), schema)
  expect_false(res$ok)
  expect_match(res$errors, "Unknown argument")
})

test_that("schema_check enforces numeric minimum/maximum ranges", {
  schema <- list(type = "object",
                  properties = list(n = list(type = "integer", minimum = 1, maximum = 10)),
                  required = list("n"))
  expect_true(schema_check(list(n = 5), schema)$ok)
  expect_false(schema_check(list(n = 100), schema)$ok)
})

test_that("guardrail_validate_call runs full validation for a real registered tool", {
  ctx <- make_test_ctx()
  sp <- make_test_spectra()
  provenance_put_object(ctx$store, "sp1", sp)

  good <- guardrail_validate_call("calculate_qc_metrics", list(spectra_id = "sp1"), ctx)
  expect_true(good$ok)

  bad_range <- guardrail_validate_call("retain_top_peaks",
    list(spectra_id = "sp1", stage_name = "x", n = 99999), ctx)
  expect_false(bad_range$ok)
})

test_that("guardrail_execute_call resolves unknown object ids as an error, not a crash", {
  ctx <- make_test_ctx()
  validated <- guardrail_validate_call("calculate_qc_metrics", list(spectra_id = "does_not_exist"), ctx)
  expect_true(validated$ok) # passes schema/domain validation; fails at execution
  res <- guardrail_execute_call(validated$tool, validated$args, ctx)
  expect_false(res$ok)
  expect_match(res$error, "Unknown object")
})

test_that("verify_narrative_grounding flags low-confidence numeric claims", {
  store <- new_provenance_store()
  provenance_add_entry(store, agent = "qc", objective = "o", plan_step = 1, tool = "calculate_qc_metrics",
                        reason = "r", arguments = list(spectra_id = "sp1", n = 42), r_function = "f", status = "ok")
  grounded <- verify_narrative_grounding("There are 42 spectra in this run.", store)
  expect_length(grounded$warnings, 0)

  ungrounded <- verify_narrative_grounding("There are 987654 spectra with 555 warnings.", store)
  expect_true(length(ungrounded$warnings) >= 1)
})
