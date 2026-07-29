#!/usr/bin/env Rscript
# scripts/generate_demo_data.R
#
# Regenerates the small demo dataset bundled in inst/extdata/. The mzML
# file is produced with Spectra's built-in MsBackendMzR backend so it
# round-trips through exactly the library the app itself uses to read
# spectra back. The CSV
# tables use a fixed deterministic formula (no RNG), so re-running this
# script reproduces the same CSV content already checked into the repo.
#
# Of the 10 MS1/MS2 pairs, the first 2 have their precursor m/z and MS2
# fragment peaks derived from real computed masses of two tryptic peptides
# in demo_proteins.fasta (one unmodified, one carrying an Oxidation-M PTM)
# so the FASTA database search feature has genuine hits to find against the
# bundled demo data. The remaining 8 pairs are arbitrary synthetic
# placeholders unrelated to any real peptide mass, as before.
#
# Requires a full Bioconductor R environment. Run via:
#   Rscript scripts/generate_demo_data.R
# or let the Docker image build run it automatically.

set.seed(1)

required_pkgs <- c("Spectra", "S4Vectors", "PSMatch", "Biostrings", "cleaver")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Missing required package(s): ", paste(missing_pkgs, collapse = ", "),
       ". Run scripts/install_packages.R first.")
}

source("R/ms_fasta_search.R")

out_dir <- "inst/extdata"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# --- Demo FASTA database (written first so it's available for the
#     self-check at the end of this script) ---
fasta_path <- file.path(out_dir, "demo_proteins.fasta")
writeLines(c(
  ">DEMO_PROT_1 Illustrative demo protein containing tryptic peptide VLDALQAIK",
  "MSDKVLDALQAIKR",
  ">DEMO_PROT_2 Illustrative demo protein containing tryptic peptide NPTIVLMDGTVK (Oxidation-M target)",
  "MADKNPTIVLMDGTVKR",
  ">DEMO_PROT_3 Distractor protein with no planted precursor match in the demo mzML",
  "MGVKAALSPTHFEDLVAQIKR"
), fasta_path)
cat("Wrote", fasta_path, "\n")

n_pairs <- 10L
rt <- seq(30, 600, length.out = n_pairs)
peptide_masses <- 400 + 50 * seq_len(n_pairs)

# Planted targets for pairs 1-2: real computed masses + real fragment ions.
target_seqs <- c("VLDALQAIK", "NPTIVLMDGTVK")
target_var_mod_mass <- c(0, VARIABLE_PTMS[["Oxidation (M)"]]$mass)
target_mods <- list(FIXED_MODS_DEFAULT, c(FIXED_MODS_DEFAULT, M = VARIABLE_PTMS[["Oxidation (M)"]]$mass))
target_charge <- 2L
target_neutral_mass <- vapply(seq_along(target_seqs), function(i) {
  peptide_neutral_mass(target_seqs[i], var_mod_mass = target_var_mod_mass[i])
}, numeric(1))
target_precursor_mz <- (target_neutral_mass + target_charge * PROTON_MASS) / target_charge
peptide_masses[1:2] <- target_precursor_mz

ms_level <- integer(0); rtime <- numeric(0); polarity <- integer(0)
precursor_mz <- numeric(0); precursor_charge <- integer(0); precursor_intensity <- numeric(0)
peaks_mz <- list(); peaks_int <- list()

for (i in seq_len(n_pairs)) {
  # MS1 scan: background peaks plus the isotope envelope of the upcoming precursor
  mz1 <- sort(c(peptide_masses[i], peptide_masses[i] + 1.0034, peptide_masses[i] + 2.0068,
                stats::runif(5, 300, 1200)))
  int1 <- round(stats::runif(length(mz1), 1e4, 1e6), 1)
  ms_level <- c(ms_level, 1L); rtime <- c(rtime, rt[i]); polarity <- c(polarity, 1L)
  precursor_mz <- c(precursor_mz, NA_real_)
  precursor_charge <- c(precursor_charge, NA_integer_)
  precursor_intensity <- c(precursor_intensity, NA_real_)
  peaks_mz[[length(peaks_mz) + 1]] <- mz1
  peaks_int[[length(peaks_int) + 1]] <- int1

  # MS2 scan: fragment ion ladder for the selected precursor. Pairs 1-2 use
  # real theoretical b/y ions for the planted target peptide (plus a little
  # noise, like the rest of the demo data); pairs 3-10 stay purely synthetic.
  if (i <= 2) {
    real_frags <- PSMatch::calculateFragments(target_seqs[i], type = c("b", "y"), z = 1,
                                               modifications = target_mods[[i]], verbose = FALSE)
    mz2 <- sort(round(c(real_frags$mz, stats::runif(3, 100, peptide_masses[i] * 2)), 4))
    int2 <- round(stats::runif(length(mz2), 1e3, 5e5), 1)
    charge_i <- target_charge
  } else {
    n_frag <- 8L
    mz2 <- sort(round(stats::runif(n_frag, 100, peptide_masses[i] * 2), 4))
    int2 <- round(stats::runif(n_frag, 1e3, 5e5), 1)
    charge_i <- if (i %% 2 == 0) 2L else 3L
  }
  ms_level <- c(ms_level, 2L); rtime <- c(rtime, rt[i] + 0.5); polarity <- c(polarity, 1L)
  precursor_mz <- c(precursor_mz, peptide_masses[i])
  precursor_charge <- c(precursor_charge, charge_i)
  precursor_intensity <- c(precursor_intensity, max(int1))
  peaks_mz[[length(peaks_mz) + 1]] <- mz2
  peaks_int[[length(peaks_int) + 1]] <- int2
}

spd <- S4Vectors::DataFrame(
  msLevel = ms_level, rtime = rtime, polarity = polarity,
  precursorMz = precursor_mz, precursorCharge = precursor_charge,
  precursorIntensity = precursor_intensity, centroided = TRUE
)
spd$mz <- peaks_mz
spd$intensity <- peaks_int

sp <- Spectra::Spectra(spd)

mzml_path <- file.path(out_dir, "demo_lcmsms.mzML")
Spectra::export(sp, Spectra::MsBackendMzR(), file = mzml_path, format = "mzML")

# Self-check: fail loudly (non-zero exit) if the written file does not
# read back with the expected spectrum count, rather than shipping a
# silently broken demo file.
check <- Spectra::Spectra(mzml_path, source = Spectra::MsBackendMzR())
if (length(check) != length(sp)) {
  stop(sprintf("Demo mzML self-check failed: wrote %d spectra but read back %d.", length(sp), length(check)))
}
cat(sprintf("Wrote %s (%d spectra, self-check passed)\n", mzml_path, length(check)))

# Second self-check: confirm the planted FASTA/mzML pair actually produces
# real hits via the full search pipeline, so a future accidental edit to
# either demo file fails the build loudly instead of silently degrading
# the FASTA-search demo.
fasta_check <- import_fasta_database(fasta_path)
search_check <- run_fasta_search(fasta_check, check, variable_ptms = "Oxidation (M)")
if (search_check$summary$n_psm_matches < 2) {
  stop(sprintf("FASTA-search self-check failed: expected >= 2 planted PSM matches, got %d.",
               search_check$summary$n_psm_matches))
}
cat(sprintf("FASTA-search self-check passed (%d PSM matches).\n", search_check$summary$n_psm_matches))

# ---- CSV demo tables (deterministic, no RNG) --------------------------
n_peptides <- 24L
proteins <- sprintf("PROT%03d", seq_len(8))
peptide_ids <- sprintf("PEP%03d", seq_len(n_peptides))
protein_of <- proteins[ceiling(seq_len(n_peptides) / 3)]
samples <- c("Sample_Ctrl_1", "Sample_Ctrl_2", "Sample_Ctrl_3",
             "Sample_Treat_1", "Sample_Treat_2", "Sample_Treat_3")

base <- 1000 + 50 * seq_len(n_peptides)
ctrl <- cbind(base, base + 20, base - 20)
treat <- t(vapply(seq_len(n_peptides), function(i) {
  if (i <= 12) c(base[i] + 15, base[i] - 15, base[i] + 5)
  else { f <- round(base[i] * 1.6); c(f, f + 20, f - 20) }
}, numeric(3)))

abundance <- data.frame(peptide = peptide_ids, protein = protein_of, ctrl, treat, check.names = FALSE)
colnames(abundance) <- c("peptide", "protein", samples)
abundance[24, "Sample_Ctrl_3"] <- NA
abundance[20, "Sample_Treat_2"] <- NA
write.csv(abundance, file.path(out_dir, "demo_abundance_table.csv"), row.names = FALSE, na = "")
cat("Wrote", file.path(out_dir, "demo_abundance_table.csv"), "\n")

cat("demo_psm_table.csv and demo_sample_metadata.csv are static and already present in inst/extdata/.\n")
