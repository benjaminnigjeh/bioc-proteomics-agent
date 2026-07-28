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
