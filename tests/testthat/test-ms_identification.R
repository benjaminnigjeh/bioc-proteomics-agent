test_that("validate_psm_table accepts a well-formed table with synonym columns", {
  df <- data.frame(Sequence = c("PEPTIDEA", "PEPTIDEB"), Accession = c("PROT1", "PROT2"),
                    scan = c(2, 4), score = c(4.1, 3.2), qvalue = c(0.001, 0.02),
                    stringsAsFactors = FALSE)
  v <- validate_psm_table(df)
  expect_true(v$ok)
  expect_equal(v$column_map$peptide, "sequence")
  expect_equal(v$column_map$protein, "accession")
  expect_true("peptide" %in% colnames(v$mapped_df))
})

test_that("validate_psm_table rejects a table missing required fields", {
  df <- data.frame(scan = c(1, 2), score = c(1, 2))
  v <- validate_psm_table(df)
  expect_false(v$ok)
  expect_true(length(v$errors) > 0)
})

test_that("filter_psms filters by score and q-value", {
  df <- data.frame(peptide = paste0("PEP", 1:5), protein = paste0("PROT", 1:5),
                    score = c(1, 2, 3, 4, 5), qvalue = c(0.2, 0.1, 0.05, 0.01, 0.001))
  out <- filter_psms(df, min_score = 3, max_qvalue = 0.05)
  expect_equal(nrow(out), 2)
  expect_true(all(out$score >= 3))
  expect_true(all(out$qvalue <= 0.05))
})

test_that("summarize_identifications reports correct PSM/peptide/protein counts", {
  df <- data.frame(peptide = c("A", "A", "B", "C"), protein = c("P1", "P1", "P1", "P2"))
  s <- summarize_identifications(df)
  expect_equal(s$n_psm, 4)
  expect_equal(s$n_unique_peptides, 3)
  expect_equal(s$n_unique_proteins, 2)
})

test_that("import_psm_table auto-detects comma and tab delimiters", {
  csv_path <- tempfile(fileext = ".csv")
  writeLines(c("peptide,protein,score", "PEP1,PROT1,4.2"), csv_path)
  on.exit(unlink(csv_path))
  df <- import_psm_table(csv_path)
  expect_equal(attr(df, "detected_delimiter"), "comma")
  expect_equal(nrow(df), 1)
})
