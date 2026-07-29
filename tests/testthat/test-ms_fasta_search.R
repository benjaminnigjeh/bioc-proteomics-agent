test_that("validate_fasta_database accepts a well-formed AAStringSet and rejects an empty one", {
  skip_if_not_installed("Biostrings")
  aa <- Biostrings::AAStringSet(c(P1 = "MSDKVLDALQAIKR", P2 = "MADKNPTIVLMDGTVKR"))
  v <- validate_fasta_database(aa)
  expect_true(v$ok)
  expect_equal(v$n_proteins, 2)

  empty <- Biostrings::AAStringSet(character(0))
  v2 <- validate_fasta_database(empty)
  expect_false(v2$ok)
})

test_that("digest_peptides produces expected tryptic peptides at 0 and 1 missed cleavages", {
  skip_if_not_installed("cleaver")
  skip_if_not_installed("Biostrings")
  aa <- Biostrings::AAStringSet(c(P1 = "MSDKVLDALQAIKR"))

  d0 <- digest_peptides(aa, missed_cleavages = 0, min_length = 1, max_length = 100)
  expect_true("VLDALQAIK" %in% d0$peptide)
  expect_true("MSDK" %in% d0$peptide)

  d1 <- digest_peptides(aa, missed_cleavages = 0:1, min_length = 1, max_length = 100)
  expect_true(nrow(d1) > nrow(d0))
})

test_that("peptide_neutral_mass matches an independently computed expected value", {
  skip_if_not_installed("PSMatch")
  # Independently computed from PSMatch::getAminoAcids() residue masses,
  # not by calling the function under test.
  residues <- c(P = 97.05276, E = 129.04259, T = 101.04768, I = 113.08406, D = 115.02694)
  expected <- residues["P"] + residues["E"] + residues["P"] + residues["T"] +
    residues["I"] + residues["D"] + residues["E"] + WATER_MASS
  expect_equal(unname(peptide_neutral_mass("PEPTIDE")), unname(expected), tolerance = 1e-3)
})

test_that("generate_candidates expands one unmodified plus one candidate per PTM-eligible residue", {
  peptide_df <- data.frame(peptide = "MAMA", protein = "P1", stringsAsFactors = FALSE)
  cand <- generate_candidates(peptide_df, variable_ptms = "Oxidation (M)")
  expect_equal(nrow(cand), 3) # 1 unmodified + 2 (one per M)
  expect_equal(sum(is.na(cand$mod_name)), 1)
  expect_equal(sum(cand$mod_name == "Oxidation (M)", na.rm = TRUE), 2)
})

test_that("generate_candidates rejects unknown variable PTM names", {
  peptide_df <- data.frame(peptide = "MAMA", protein = "P1", stringsAsFactors = FALSE)
  expect_error(generate_candidates(peptide_df, variable_ptms = "NotAPTM"))
})

test_that("generate_candidates errors past max_candidates instead of hanging", {
  peptide_df <- data.frame(peptide = c("MAMA", "MAMA"), protein = c("P1", "P2"), stringsAsFactors = FALSE)
  expect_error(generate_candidates(peptide_df, variable_ptms = "Oxidation (M)", max_candidates = 2))
})

test_that("run_fasta_search finds a planted precursor+fragment match and the result feeds the existing PSM pipeline unmodified", {
  skip_if_not_installed("Biostrings")
  skip_if_not_installed("cleaver")
  skip_if_not_installed("PSMatch")

  target <- "VLDALQAIK"
  aa <- Biostrings::AAStringSet(c(DEMO = "MSDKVLDALQAIKR"))

  neutral_mass <- peptide_neutral_mass(target)
  charge <- 2L
  precursor_mz <- (neutral_mass + charge * PROTON_MASS) / charge
  frags <- PSMatch::calculateFragments(target, type = c("b", "y"), z = 1,
                                        modifications = FIXED_MODS_DEFAULT, verbose = FALSE)

  spd <- S4Vectors::DataFrame(
    msLevel = c(1L, 2L), rtime = c(1.0, 1.5), polarity = c(1L, 1L),
    precursorMz = c(NA_real_, precursor_mz), precursorCharge = c(NA_integer_, charge),
    centroided = TRUE
  )
  spd$mz <- list(c(100.0, 200.0), sort(frags$mz))
  spd$intensity <- list(c(1000, 2000), rep(1e5, length(frags$mz)))
  sp <- Spectra::Spectra(spd)

  res <- run_fasta_search(aa, sp, missed_cleavages = 0, min_length = 4, max_length = 40)
  expect_true(nrow(res$psm_df) >= 1)
  expect_true(target %in% res$psm_df$peptide)
  expect_true(all(is.na(res$psm_df$qvalue)))
  expect_equal(res$summary$n_psm_matches, nrow(res$psm_df))

  # Reuse the existing (unmodified) identification pipeline on the output.
  filtered <- filter_psms(res$psm_df, min_score = 0)
  expect_equal(nrow(filtered), nrow(res$psm_df))
  s <- summarize_identifications(res$psm_df)
  expect_equal(s$n_psm, nrow(res$psm_df))
})
