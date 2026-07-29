test_that("all expected tools are registered", {
  expected <- c(
    "inspect_uploaded_file", "import_ms_file", "summarize_experiment", "list_spectra", "get_spectrum",
    "calculate_qc_metrics", "calculate_standardized_qc_metrics", "plot_tic", "plot_bpc", "plot_spectrum",
    "filter_spectra", "normalize_spectra", "retain_top_peaks", "compare_spectra",
    "import_psm_table", "filter_psms", "summarize_identifications",
    "import_quant_table", "build_qfeatures", "normalize_quantification", "aggregate_to_proteins",
    "run_exploratory_analysis", "plot_quantification_heatmap", "import_sample_metadata",
    "run_msstats_comparison", "generate_report"
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

test_that("calculate_standardized_qc_metrics tool returns standardized MsQuality metrics", {
  skip_if_not_installed("MsQuality")
  ctx <- make_test_ctx()
  provenance_put_object(ctx$store, "sp1", make_test_spectra())
  tool <- get_tool("calculate_standardized_qc_metrics")
  res <- guardrail_execute_call(tool, list(spectra_id = "sp1"), ctx)
  expect_true(res$ok)
  expect_true(length(res$output) > 0)
})

test_that("plot_quantification_heatmap tool saves a PNG artifact", {
  skip_if_not_installed("ComplexHeatmap")
  ctx <- make_test_ctx()
  df <- data.frame(peptide = paste0("PEP", 1:6),
                    s1 = 1:6, s2 = 2:7, s3 = 3:8, s4 = 4:9)
  qf <- build_qfeatures(df, "peptide", c("s1", "s2", "s3", "s4"), assay_name = "peptides")
  provenance_put_object(ctx$store, "qf1", qf)

  tool <- get_tool("plot_quantification_heatmap")
  res <- guardrail_execute_call(tool, list(qfeatures_id = "qf1", assay_name = "peptides"), ctx)
  expect_true(res$ok)
  expect_true(file.exists(res$artifacts))
})

test_that("import_sample_metadata tool validates and stores sample metadata", {
  store <- new_provenance_store()
  dir <- tempfile("bpa_test_")
  dir.create(dir)
  uploads <- new.env()
  csv_path <- file.path(dir, "meta.csv")
  utils::write.csv(data.frame(sample_id = c("s1", "s2"), group = c("A", "B")), csv_path, row.names = FALSE)
  file_id <- register_upload(uploads, csv_path, "meta.csv")
  ctx <- new_session_ctx(store, dir, uploads, report_params_builder = function(objective) list(objective = objective))

  tool <- get_tool("import_sample_metadata")
  res <- guardrail_execute_call(tool, list(file_id = file_id), ctx)
  expect_true(res$ok)
  stored <- provenance_get_object(store, res$output_id)
  expect_equal(nrow(stored), 2)
})

test_that("run_msstats_comparison tool runs an end-to-end comparison and stores the result", {
  skip_if_not_installed("MSstats")
  ctx <- make_test_ctx()
  df <- data.frame(
    peptide = paste0("PEP", 1:6), protein = rep(c("PROT1", "PROT2"), each = 3),
    s1 = c(1000, 1100, 1150, 1200, 1250, 1300), s2 = c(1020, 1120, 1170, 1220, 1270, 1320),
    s3 = c(980, 1080, 1130, 1180, 1230, 1280), s4 = c(700, 800, 850, 900, 950, 1000),
    s5 = c(690, 790, 840, 890, 940, 990), s6 = c(710, 810, 860, 910, 960, 1010)
  )
  meta <- data.frame(sample_id = c("s1", "s2", "s3", "s4", "s5", "s6"),
                      group = c("Ctrl", "Ctrl", "Ctrl", "Treat", "Treat", "Treat"))
  provenance_put_object(ctx$store, "tbl1", df)
  provenance_put_object(ctx$store, "meta1", meta)

  tool <- get_tool("run_msstats_comparison")
  args <- list(table_id = "tbl1", protein_col = "protein", peptide_col = "peptide",
               sample_cols = as.list(c("s1", "s2", "s3", "s4", "s5", "s6")),
               metadata_id = "meta1", group_col = "group")
  res <- guardrail_execute_call(tool, args, ctx)
  expect_true(res$ok)
  expect_equal(res$output$n_proteins, 2)
  stored <- provenance_get_object(ctx$store, res$output_id)
  expect_true(all(c("Protein", "log2FC", "adj.pvalue") %in% colnames(stored)))
})
