# R/ms_fasta_search.R
#
# Deterministic in-silico database search: FASTA import, tryptic digestion,
# peptide/PTM candidate mass generation, precursor-mass matching against a
# Spectra object, and b/y fragment-ion scoring. Output is shaped exactly
# like the canonical PSM data.frame consumed by R/ms_identification.R, so
# a search result can flow through filter_psms()/summarize_identifications()
# unmodified.
#
# This is an approximate, exploratory search: no decoy database, no FDR
# control. `score` is the fraction of theoretical b/y ions matched to
# observed peaks, for ranking only; `qvalue` is intentionally NA.

FIXED_MODS_DEFAULT <- c(C = 57.02146)

VARIABLE_PTMS <- list(
  "Oxidation (M)"    = list(residue = "M",           mass = 15.9949),
  "Phospho (STY)"     = list(residue = c("S", "T", "Y"), mass = 79.9663),
  "Acetyl (K)"        = list(residue = "K",           mass = 42.0106),
  "Deamidation (NQ)"  = list(residue = c("N", "Q"),   mass = 0.9840)
)

WATER_MASS <- 18.010565
PROTON_MASS <- 1.007276

#' Read a protein FASTA database. Protein names are truncated to the first
#' whitespace-delimited token of each header line (standard accession
#' convention), so free-text descriptions don't leak into downstream
#' protein-id columns.
#' @export
import_fasta_database <- function(path) {
  if (!requireNamespace("Biostrings", quietly = TRUE)) stop("Biostrings package required.")
  aa <- Biostrings::readAAStringSet(path)
  names(aa) <- sub("\\s.*$", "", names(aa))
  aa
}

#' Validate an imported FASTA database: non-empty, plausible amino-acid
#' sequences only.
#' @export
validate_fasta_database <- function(aa) {
  errors <- character(0)
  n_proteins <- length(aa)
  if (n_proteins == 0) {
    errors <- c(errors, "FASTA database contains no sequences.")
  } else {
    seqs <- as.character(aa)
    if (any(!nzchar(seqs))) errors <- c(errors, "FASTA database contains empty sequence(s).")
    bad_chars <- grepl("[^ACDEFGHIKLMNPQRSTVWYX]", seqs)
    if (any(bad_chars)) errors <- c(errors, "FASTA database contains non-standard amino-acid character(s).")
  }
  list(ok = length(errors) == 0, errors = errors, n_proteins = n_proteins)
}

#' In-silico enzymatic digestion of a protein FASTA database into candidate
#' peptides, length-filtered and deduplicated per (peptide, protein) pair.
#' @export
digest_peptides <- function(aa, enzyme = "trypsin", missed_cleavages = 0:1,
                             min_length = 6, max_length = 40) {
  if (!requireNamespace("cleaver", quietly = TRUE)) stop("cleaver package required.")
  cleaved <- cleaver::cleave(aa, enzym = enzyme, missedCleavages = missed_cleavages)
  flat <- unlist(cleaved)
  df <- data.frame(peptide = as.character(flat), protein = names(flat), stringsAsFactors = FALSE)
  nch <- nchar(df$peptide)
  df <- df[nch >= min_length & nch <= max_length, , drop = FALSE]
  df <- unique(df)
  rownames(df) <- NULL
  df
}

#' Monoisotopic neutral mass of a peptide sequence, given fixed
#' (applied to every occurrence of the residue) and an optional flat
#' variable-modification mass addition (already resolved for one site).
#' @export
peptide_neutral_mass <- function(sequence, fixed_mods = FIXED_MODS_DEFAULT, var_mod_mass = 0) {
  if (!requireNamespace("PSMatch", quietly = TRUE)) stop("PSMatch package required.")
  aa_table <- PSMatch::getAminoAcids()
  residues <- strsplit(sequence, "")[[1]]
  residue_mass <- stats::setNames(aa_table$ResidueMass, aa_table$AA)
  masses <- residue_mass[residues]
  if (anyNA(masses)) stop(sprintf("Unrecognized residue(s) in sequence '%s'.", sequence))
  fixed_shift <- vapply(residues, function(r) if (r %in% names(fixed_mods)) fixed_mods[[r]] else 0, numeric(1))
  sum(masses) + sum(fixed_shift) + WATER_MASS + var_mod_mass
}

#' Expand a digested-peptide data.frame into search candidates: one
#' unmodified row per peptide, plus one row per matching-residue occurrence
#' for each selected variable PTM. Errors loudly if the candidate count
#' would exceed `max_candidates` (guards against combinatorial blow-up on
#' an arbitrary uploaded FASTA).
#' @export
generate_candidates <- function(peptide_df, variable_ptms = character(0),
                                 fixed_mods = FIXED_MODS_DEFAULT, max_candidates = 20000L) {
  unknown <- setdiff(variable_ptms, names(VARIABLE_PTMS))
  if (length(unknown) > 0) stop(sprintf("Unknown variable PTM(s): %s.", paste(unknown, collapse = ", ")))

  rows <- vector("list", nrow(peptide_df) * (1 + length(variable_ptms) * 3))
  n <- 0L
  add_row <- function(peptide, protein, mod_name, mod_residue, mod_position, var_mod_mass) {
    # Peptides containing non-standard residues (e.g. 'X') have no defined
    # mass and are skipped rather than aborting the whole search -- FASTA
    # files with ambiguous residues are common enough that this shouldn't
    # be fatal.
    mass <- tryCatch(peptide_neutral_mass(peptide, fixed_mods, var_mod_mass), error = function(e) NA_real_)
    if (is.na(mass)) return(invisible(NULL))
    n <<- n + 1L
    if (n > max_candidates) {
      stop(sprintf(
        "Candidate count exceeds the limit of %d. Reduce missed cleavages, variable PTMs, or the peptide length range, or upload a smaller FASTA.",
        max_candidates))
    }
    rows[[n]] <<- data.frame(
      peptide = peptide, protein = protein,
      mod_name = mod_name, mod_residue = mod_residue, mod_position = mod_position,
      neutral_mass = mass,
      stringsAsFactors = FALSE
    )
  }

  for (i in seq_len(nrow(peptide_df))) {
    peptide <- peptide_df$peptide[i]
    protein <- peptide_df$protein[i]
    add_row(peptide, protein, NA_character_, NA_character_, NA_integer_, 0)
    residues <- strsplit(peptide, "")[[1]]
    for (mod_name in variable_ptms) {
      ptm <- VARIABLE_PTMS[[mod_name]]
      positions <- which(residues %in% ptm$residue)
      for (pos in positions) {
        add_row(peptide, protein, mod_name, residues[pos], pos, ptm$mass)
      }
    }
  }
  if (n == 0) {
    return(data.frame(peptide = character(0), protein = character(0),
                       mod_name = character(0), mod_residue = character(0),
                       mod_position = integer(0), neutral_mass = numeric(0),
                       stringsAsFactors = FALSE))
  }
  do.call(rbind, rows[seq_len(n)])
}

#' Match candidate peptide masses against the precursor mass of every MS2
#' scan in a Spectra object, within a ppm tolerance. Returns a long
#' data.frame of (scan, charge, candidate row index) pairs.
#' @export
match_precursor_masses <- function(candidates, sp, precursor_ppm = 10) {
  ms2_idx <- which(Spectra::msLevel(sp) == 2)
  if (length(ms2_idx) == 0 || nrow(candidates) == 0) {
    return(data.frame(scan = integer(0), charge = integer(0), candidate_row = integer(0)))
  }
  scans <- Spectra::acquisitionNum(sp)[ms2_idx]
  mzs <- Spectra::precursorMz(sp)[ms2_idx]
  charges <- Spectra::precursorCharge(sp)[ms2_idx]

  out <- vector("list", length(ms2_idx))
  for (i in seq_along(ms2_idx)) {
    z_candidates <- if (is.na(charges[i])) 2:3 else charges[i]
    for (z in z_candidates) {
      if (is.na(mzs[i])) next
      obs_mass <- mzs[i] * z - z * PROTON_MASS
      ppm_err <- abs(obs_mass - candidates$neutral_mass) / candidates$neutral_mass * 1e6
      hits <- which(ppm_err <= precursor_ppm)
      if (length(hits) > 0) {
        out[[i]] <- rbind(out[[i]], data.frame(
          scan = scans[i], charge = z, candidate_row = hits, spectra_idx = ms2_idx[i]
        ))
      }
    }
  }
  res <- do.call(rbind, out)
  if (is.null(res)) res <- data.frame(scan = integer(0), charge = integer(0), candidate_row = integer(0), spectra_idx = integer(0))
  res
}

#' Score a candidate peptide (with at most one variable PTM, applied per
#' PSMatch::calculateFragments()'s residue-wide convention -- not strictly
#' positional) against the observed peaks of one spectrum: fraction of
#' theoretical b/y ions matched within a Da tolerance.
#' @export
score_candidate_fragments <- function(sequence, mod_name = NA_character_, mod_residue = NA_character_,
                                       fixed_mods = FIXED_MODS_DEFAULT, peaks_df, fragment_tol_da = 0.05) {
  if (!requireNamespace("PSMatch", quietly = TRUE)) stop("PSMatch package required.")
  mods <- fixed_mods
  if (!is.na(mod_name)) {
    ptm <- VARIABLE_PTMS[[mod_name]]
    mods[mod_residue] <- ptm$mass
  }
  frags <- PSMatch::calculateFragments(sequence, type = c("b", "y"), z = 1, modifications = mods, verbose = FALSE)
  if (nrow(frags) == 0 || nrow(peaks_df) == 0) return(0)
  matched <- vapply(frags$mz, function(m) any(abs(peaks_df[, "mz"] - m) <= fragment_tol_da), logical(1))
  sum(matched) / nrow(frags)
}

#' Run an in-silico database search: digest -> candidate masses -> precursor
#' match -> fragment score -> best-scoring candidate per matched spectrum.
#' Returns a canonical PSM-shaped data.frame plus a small numeric summary.
#' @export
run_fasta_search <- function(aa, sp, enzyme = "trypsin", missed_cleavages = 0:1,
                              min_length = 6, max_length = 40, variable_ptms = character(0),
                              precursor_ppm = 10, fragment_tol_da = 0.05,
                              fixed_mods = FIXED_MODS_DEFAULT, min_score = 0.2,
                              max_candidates = 20000L) {
  peptide_df <- digest_peptides(aa, enzyme = enzyme, missed_cleavages = missed_cleavages,
                                 min_length = min_length, max_length = max_length)
  candidates <- generate_candidates(peptide_df, variable_ptms = variable_ptms,
                                     fixed_mods = fixed_mods, max_candidates = max_candidates)
  matches <- match_precursor_masses(candidates, sp, precursor_ppm = precursor_ppm)

  psm_rows <- list()
  if (nrow(matches) > 0) {
    # Group by spectrum index, not `scan` (== acquisitionNum): acquisitionNum
    # is only populated after an mzML export/import round-trip and is NA for
    # in-memory-constructed Spectra objects, so it cannot be relied on as a
    # join key. `scan` is still reported in the output PSM table for display
    # / compatibility with the rest of the identification pipeline.
    for (idx in unique(matches$spectra_idx)) {
      scan_matches <- matches[matches$spectra_idx == idx, , drop = FALSE]
      peaks_df <- Spectra::peaksData(sp[idx])[[1]]
      scores <- vapply(seq_len(nrow(scan_matches)), function(j) {
        cand <- candidates[scan_matches$candidate_row[j], ]
        score_candidate_fragments(cand$peptide, cand$mod_name, cand$mod_residue,
                                   fixed_mods, peaks_df, fragment_tol_da)
      }, numeric(1))
      best <- which.max(scores)
      if (scores[best] >= min_score) {
        cand <- candidates[scan_matches$candidate_row[best], ]
        psm_rows[[length(psm_rows) + 1]] <- data.frame(
          peptide = cand$peptide, protein = cand$protein, scan = scan_matches$scan[best],
          charge = scan_matches$charge[best], score = scores[best], qvalue = NA_real_,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  psm_df <- if (length(psm_rows) > 0) do.call(rbind, psm_rows) else {
    data.frame(peptide = character(0), protein = character(0), scan = integer(0),
               charge = integer(0), score = numeric(0), qvalue = numeric(0))
  }

  list(
    psm_df = psm_df,
    summary = list(
      n_candidate_peptides = nrow(peptide_df),
      n_psm_matches = nrow(psm_df),
      n_unique_peptides = length(unique(psm_df$peptide)),
      n_unique_proteins = length(unique(psm_df$protein))
    )
  )
}
