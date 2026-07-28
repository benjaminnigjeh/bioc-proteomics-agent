# R/ms_quantification.R
#
# Deterministic quantification handling for the Quantification Agent.
# Builds and manipulates a QFeatures object from an imported abundance
# table (peptide- or protein-level), and provides missingness,
# normalization, aggregation, PCA, and a simple two-group comparison.

#' Import a peptide/protein abundance table (CSV/TSV).
#' @export
import_quant_table <- function(path) {
  first_line <- readLines(path, n = 1, warn = FALSE)
  delim <- if (lengths(regmatches(first_line, gregexpr("\t", first_line))) >
               lengths(regmatches(first_line, gregexpr(",", first_line)))) "\t" else ","
  utils::read.delim(path, sep = delim, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Validate an abundance table: requires an identifier column and at least
#' two numeric sample columns.
#'
#' @param df data.frame as imported
#' @param id_col character name of the identifier column (peptide/protein id)
#' @export
validate_quant_table <- function(df, id_col = NULL) {
  errors <- character(0)
  if (is.null(id_col)) {
    id_col <- colnames(df)[1]
  }
  if (!id_col %in% colnames(df)) {
    errors <- c(errors, sprintf("Identifier column '%s' not found.", id_col))
  }
  numeric_cols <- colnames(df)[vapply(df, is.numeric, logical(1))]
  numeric_cols <- setdiff(numeric_cols, id_col)
  if (length(numeric_cols) < 2) {
    errors <- c(errors, "At least two numeric sample columns are required for quantification.")
  }
  if (anyDuplicated(df[[id_col]]) > 0 && length(errors) == 0) {
    errors <- c(errors, sprintf("Duplicate identifiers found in column '%s'.", id_col))
  }
  list(ok = length(errors) == 0, errors = errors, id_col = id_col, sample_cols = numeric_cols)
}

#' Build a QFeatures object from a validated abundance table.
#'
#' @param df data.frame
#' @param id_col identifier column name
#' @param sample_cols numeric sample column names
#' @param assay_name name for the assay level, e.g. "peptides" or "proteins"
#' @param sample_metadata optional data.frame with a `sample_id` column
#'   matching sample_cols, plus any grouping columns
#' @export
build_qfeatures <- function(df, id_col, sample_cols, assay_name = "peptides",
                             sample_metadata = NULL) {
  if (!requireNamespace("QFeatures", quietly = TRUE)) stop("QFeatures package required.")
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) stop("SummarizedExperiment package required.")

  mat <- as.matrix(df[, sample_cols, drop = FALSE])
  rownames(mat) <- make.unique(as.character(df[[id_col]]))
  mode(mat) <- "numeric"

  col_data <- if (!is.null(sample_metadata)) {
    sm <- sample_metadata[match(sample_cols, sample_metadata$sample_id), , drop = FALSE]
    rownames(sm) <- sample_cols
    sm
  } else {
    data.frame(sample_id = sample_cols, row.names = sample_cols, stringsAsFactors = FALSE)
  }

  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(intensity = mat),
    colData = col_data
  )
  qf <- QFeatures::QFeatures(list(se), colData = col_data)
  names(qf)[1] <- assay_name
  qf
}

#' Summarize missingness (NA / zero) per sample and overall for a QFeatures
#' assay.
#' @export
summarize_missingness <- function(qf, assay_name) {
  mat <- SummarizedExperiment::assay(qf[[assay_name]])
  n_total <- length(mat)
  n_na <- sum(is.na(mat))
  per_sample <- colSums(is.na(mat))
  per_feature <- rowSums(is.na(mat))
  list(
    total_values = n_total,
    n_missing = as.integer(n_na),
    pct_missing = 100 * n_na / n_total,
    missing_per_sample = as.list(per_sample),
    features_fully_missing = sum(per_feature == ncol(mat))
  )
}

#' Log2-transform an assay (adding a pseudocount to zeros if requested).
#' @export
transform_quantification <- function(qf, assay_name, method = c("log2", "none"), pseudocount = 1) {
  method <- match.arg(method)
  if (method == "none") return(qf)
  mat <- SummarizedExperiment::assay(qf[[assay_name]])
  mat[mat == 0] <- NA
  log_mat <- log2(mat)
  new_name <- paste0(assay_name, "_log2")
  qf <- QFeatures::addAssay(qf, SummarizedExperiment::SummarizedExperiment(
    assays = list(intensity = log_mat), colData = SummarizedExperiment::colData(qf[[assay_name]])
  ), name = new_name)
  qf
}

#' Normalize an assay using a QFeatures/MsCoreUtils-supported method
#' (default: median centering).
#' @export
normalize_quantification <- function(qf, assay_name, method = "center.median") {
  new_name <- paste0(assay_name, "_norm")
  qf <- QFeatures::normalize(qf, i = assay_name, name = new_name, method = method)
  qf
}

#' Aggregate a peptide-level assay to protein level using an identifier
#' mapping column (fcol) present in the row data.
#' @export
aggregate_to_proteins <- function(qf, assay_name, fcol = "protein", fun = MsCoreUtils::medianPolish) {
  new_name <- paste0(assay_name, "_proteins")
  qf <- QFeatures::aggregateFeatures(qf, i = assay_name, fcol = fcol, name = new_name, fun = fun)
  qf
}

#' Compute a PCA on an assay (samples as points) when there are enough
#' complete-case features. Returns NULL-safe list with scores + variance
#' explained, or ok = FALSE if not valid.
#' @export
run_pca <- function(qf, assay_name, ncomp = 2) {
  mat <- SummarizedExperiment::assay(qf[[assay_name]])
  complete <- mat[stats::complete.cases(mat), , drop = FALSE]
  if (nrow(complete) < 3 || ncol(complete) < 3) {
    return(list(ok = FALSE, reason = "Not enough complete-case features or samples for PCA (need >= 3 of each)."))
  }
  pca <- stats::prcomp(t(complete), scale. = TRUE)
  var_explained <- (pca$sdev^2) / sum(pca$sdev^2)
  scores <- as.data.frame(pca$x[, seq_len(min(ncomp, ncol(pca$x))), drop = FALSE])
  scores$sample_id <- rownames(scores)
  list(ok = TRUE, scores = scores, variance_explained = var_explained[seq_len(min(ncomp, length(var_explained)))])
}

#' Simple two-group comparison (moderated t-test via limma if available,
#' else Welch's t-test per feature) when group sizes are statistically
#' minimally valid (>= 2 replicates per group).
#' @export
run_two_group_comparison <- function(qf, assay_name, group) {
  mat <- SummarizedExperiment::assay(qf[[assay_name]])
  group <- factor(group)
  if (nlevels(group) != 2) {
    return(list(ok = FALSE, reason = "Two-group comparison requires exactly two groups."))
  }
  tab <- table(group)
  if (any(tab < 2)) {
    return(list(ok = FALSE, reason = "Each group needs at least 2 replicates for a valid comparison."))
  }

  if (requireNamespace("limma", quietly = TRUE)) {
    design <- stats::model.matrix(~group)
    fit <- limma::lmFit(mat, design)
    fit <- limma::eBayes(fit)
    res <- limma::topTable(fit, coef = 2, number = Inf, sort.by = "none")
    res$feature_id <- rownames(mat)
    res$p.value <- res$P.Value
    return(list(ok = TRUE, method = "limma_moderated_t", results = res))
  }

  # Fallback: per-feature Welch's t-test
  g1 <- mat[, group == levels(group)[1], drop = FALSE]
  g2 <- mat[, group == levels(group)[2], drop = FALSE]
  res <- do.call(rbind, lapply(seq_len(nrow(mat)), function(i) {
    x <- g1[i, ]; y <- g2[i, ]
    if (sum(!is.na(x)) < 2 || sum(!is.na(y)) < 2) {
      return(data.frame(feature_id = rownames(mat)[i], logFC = NA_real_, p.value = NA_real_))
    }
    tt <- tryCatch(stats::t.test(x, y), error = function(e) NULL)
    data.frame(feature_id = rownames(mat)[i],
               logFC = mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE),
               p.value = if (is.null(tt)) NA_real_ else tt$p.value)
  }))
  res$adj.P.Val <- stats::p.adjust(res$p.value, method = "BH")
  list(ok = TRUE, method = "welch_t_test", results = res)
}
