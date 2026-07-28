#!/usr/bin/env Rscript
# scripts/generate_demo_data.R
#
# Regenerates the small demo dataset bundled in inst/extdata/. The mzML
# file is produced with Spectra + MsBackendMzR so it round-trips through
# exactly the library the app itself uses to read spectra back. The CSV
# tables use a fixed deterministic formula (no RNG), so re-running this
# script reproduces the same CSV content already checked into the repo.
#
# Requires a full Bioconductor R environment. Run via:
#   Rscript scripts/generate_demo_data.R
# or let the Docker image build run it automatically.

set.seed(1)

required_pkgs <- c("Spectra", "MsBackendMzR", "S4Vectors")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Missing required package(s): ", paste(missing_pkgs, collapse = ", "),
       ". Run scripts/install_packages.R first.")
}

out_dir <- "inst/extdata"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

n_pairs <- 10L
rt <- seq(30, 600, length.out = n_pairs)
peptide_masses <- 400 + 50 * seq_len(n_pairs)

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

  # MS2 scan: synthetic fragment ion ladder for the selected precursor
  n_frag <- 8L
  mz2 <- sort(round(stats::runif(n_frag, 100, peptide_masses[i] * 2), 4))
  int2 <- round(stats::runif(n_frag, 1e3, 5e5), 1)
  ms_level <- c(ms_level, 2L); rtime <- c(rtime, rt[i] + 0.5); polarity <- c(polarity, 1L)
  precursor_mz <- c(precursor_mz, peptide_masses[i])
  precursor_charge <- c(precursor_charge, if (i %% 2 == 0) 2L else 3L)
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
Spectra::export(sp, MsBackendMzR::MsBackendMzR(), file = mzml_path, format = "mzML")

# Self-check: fail loudly (non-zero exit) if the written file does not
# read back with the expected spectrum count, rather than shipping a
# silently broken demo file.
check <- Spectra::Spectra(mzml_path, source = MsBackendMzR::MsBackendMzR())
if (length(check) != length(sp)) {
  stop(sprintf("Demo mzML self-check failed: wrote %d spectra but read back %d.", length(sp), length(check)))
}
cat(sprintf("Wrote %s (%d spectra, self-check passed)\n", mzml_path, length(check)))

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
