test_that("validate_quant_table requires an id column and >= 2 numeric samples", {
  df <- data.frame(peptide = c("A", "B"), s1 = c(1, 2), s2 = c(3, 4))
  v <- validate_quant_table(df)
  expect_true(v$ok)
  expect_equal(v$id_col, "peptide")
  expect_setequal(v$sample_cols, c("s1", "s2"))
})

test_that("validate_quant_table rejects tables with fewer than two numeric sample columns", {
  df <- data.frame(peptide = c("A", "B"), s1 = c(1, 2))
  v <- validate_quant_table(df)
  expect_false(v$ok)
})

test_that("validate_quant_table rejects duplicate identifiers", {
  df <- data.frame(peptide = c("A", "A"), s1 = c(1, 2), s2 = c(3, 4))
  v <- validate_quant_table(df)
  expect_false(v$ok)
})

test_that("build_qfeatures constructs a QFeatures object with the expected dimensions", {
  df <- data.frame(peptide = c("PEP1", "PEP2", "PEP3"),
                    s1 = c(10, 20, 30), s2 = c(11, 19, 29), s3 = c(9, 21, 31))
  qf <- build_qfeatures(df, "peptide", c("s1", "s2", "s3"), assay_name = "peptides")
  expect_s4_class(qf, "QFeatures")
  expect_equal(dim(qf[["peptides"]]), c(3, 3))
})

test_that("summarize_missingness counts NAs correctly", {
  df <- data.frame(peptide = c("PEP1", "PEP2"), s1 = c(10, NA), s2 = c(NA, 20))
  qf <- build_qfeatures(df, "peptide", c("s1", "s2"), assay_name = "peptides")
  m <- summarize_missingness(qf, "peptides")
  expect_equal(m$n_missing, 2)
  expect_equal(m$total_values, 4)
})

test_that("run_pca reports ok = FALSE when there are too few complete features/samples", {
  df <- data.frame(peptide = c("PEP1", "PEP2"), s1 = c(10, 20), s2 = c(11, 19))
  qf <- build_qfeatures(df, "peptide", c("s1", "s2"), assay_name = "peptides")
  p <- run_pca(qf, "peptides")
  expect_false(p$ok)
})

test_that("run_two_group_comparison requires exactly two groups with >= 2 replicates each", {
  df <- data.frame(peptide = paste0("PEP", 1:5),
                    s1 = 1:5, s2 = 2:6, s3 = 3:7, s4 = 4:8)
  qf <- build_qfeatures(df, "peptide", c("s1", "s2", "s3", "s4"), assay_name = "peptides")
  bad <- run_two_group_comparison(qf, "peptides", c("A", "A", "A", "B"))
  expect_false(bad$ok)

  ok <- run_two_group_comparison(qf, "peptides", c("A", "A", "B", "B"))
  expect_true(ok$ok)
  expect_true("results" %in% names(ok))
})

test_that("plot_quant_heatmap returns a Heatmap object for a complete-case assay", {
  skip_if_not_installed("ComplexHeatmap")
  df <- data.frame(peptide = paste0("PEP", 1:6),
                    s1 = 1:6, s2 = 2:7, s3 = 3:8, s4 = 4:9)
  qf <- build_qfeatures(df, "peptide", c("s1", "s2", "s3", "s4"), assay_name = "peptides")
  ht <- plot_quant_heatmap(qf, "peptides")
  expect_s4_class(ht, "Heatmap")
})

test_that("plot_quant_heatmap errors when no complete-case features remain", {
  df <- data.frame(peptide = c("PEP1", "PEP2"), s1 = c(NA, NA), s2 = c(NA, NA))
  qf <- build_qfeatures(df, "peptide", c("s1", "s2"), assay_name = "peptides")
  expect_error(plot_quant_heatmap(qf, "peptides"))
})

test_that("validate_sample_metadata requires sample_id and at least one other column", {
  ok <- validate_sample_metadata(data.frame(sample_id = c("s1", "s2"), group = c("A", "B")))
  expect_true(ok$ok)

  no_sample_id <- validate_sample_metadata(data.frame(id = c("s1", "s2"), group = c("A", "B")))
  expect_false(no_sample_id$ok)

  no_group <- validate_sample_metadata(data.frame(sample_id = c("s1", "s2")))
  expect_false(no_group$ok)

  dup <- validate_sample_metadata(data.frame(sample_id = c("s1", "s1"), group = c("A", "B")))
  expect_false(dup$ok)
})

test_that("run_msstats_comparison runs a real two-group comparison end-to-end", {
  skip_if_not_installed("MSstats")
  df <- data.frame(
    peptide = paste0("PEP", 1:6), protein = rep(c("PROT1", "PROT2"), each = 3),
    s1 = c(1000, 1100, 1150, 1200, 1250, 1300), s2 = c(1020, 1120, 1170, 1220, 1270, 1320),
    s3 = c(980, 1080, 1130, 1180, 1230, 1280), s4 = c(700, 800, 850, 900, 950, 1000),
    s5 = c(690, 790, 840, 890, 940, 990), s6 = c(710, 810, 860, 910, 960, 1010)
  )
  meta <- data.frame(sample_id = c("s1", "s2", "s3", "s4", "s5", "s6"),
                      group = c("Ctrl", "Ctrl", "Ctrl", "Treat", "Treat", "Treat"))
  res <- run_msstats_comparison(df, "protein", "peptide", c("s1", "s2", "s3", "s4", "s5", "s6"), meta, "group")
  expect_true(res$ok)
  expect_true(all(c("Protein", "log2FC", "pvalue", "adj.pvalue") %in% colnames(res$results)))
  expect_equal(nrow(res$results), 2)
})

test_that("run_msstats_comparison rejects fewer or more than two groups", {
  df <- data.frame(peptide = c("PEP1", "PEP2"), protein = c("PROT1", "PROT1"), s1 = c(1, 2), s2 = c(3, 4))
  meta <- data.frame(sample_id = c("s1", "s2"), group = c("Ctrl", "Ctrl"))
  res <- run_msstats_comparison(df, "protein", "peptide", c("s1", "s2"), meta, "group")
  expect_false(res$ok)
})
