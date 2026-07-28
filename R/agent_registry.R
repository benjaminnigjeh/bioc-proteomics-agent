# R/agent_registry.R
#
# The restricted tool registry. Every capability Claude can invoke is
# declared here as a `tool_spec`: name, description, JSON input schema,
# a domain-specific R validation function, a deterministic R execution
# function, an output schema, resource limits, expected artifacts, and a
# provenance template. Claude NEVER executes R code directly -- it can
# only request one of these named tools, and the app decides whether and
# how to run it (see agent_guardrails.R / agent_execution.R).
#
# Object handles: large Bioconductor objects (Spectra, MsExperiment,
# QFeatures, data.frames) are never sent to Claude. They are stored in the
# session's provenance object store under a short string id (e.g.
# "spectra_raw", "spectra_filtered_1"), and tools reference them by id.

TOOL_REGISTRY <- new.env(parent = emptyenv())

#' Register a tool spec into the global registry.
#' @export
register_tool <- function(spec) {
  required_fields <- c("name", "description", "input_schema", "validate", "execute",
                        "output_schema", "timeout_s", "expected_artifacts")
  missing <- setdiff(required_fields, names(spec))
  if (length(missing) > 0) {
    stop(sprintf("Tool spec '%s' missing fields: %s", spec$name %||% "?", paste(missing, collapse = ", ")))
  }
  TOOL_REGISTRY[[spec$name]] <- spec
  invisible(spec)
}

#' @export
get_tool <- function(name) {
  if (!exists(name, envir = TOOL_REGISTRY, inherits = FALSE)) return(NULL)
  get(name, envir = TOOL_REGISTRY, inherits = FALSE)
}

#' @export
list_tool_names <- function() {
  sort(ls(envir = TOOL_REGISTRY))
}

#' Tool list formatted for the Claude Messages API `tools` parameter.
#' @export
list_tools_for_claude <- function() {
  lapply(list_tool_names(), function(nm) {
    spec <- get_tool(nm)
    list(name = spec$name, description = spec$description, input_schema = spec$input_schema)
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ---------------------------------------------------------------------------
# Helpers shared across tool executors
# ---------------------------------------------------------------------------

.resolve_object <- function(ctx, id, expected_class = NULL) {
  obj <- provenance_get_object(ctx$store, id)
  if (is.null(obj)) stop(sprintf("Unknown object id '%s'. It may not have been created yet.", id))
  if (!is.null(expected_class) && !inherits(obj, expected_class)) {
    stop(sprintf("Object '%s' is not of the expected type '%s'.", id, expected_class))
  }
  obj
}

.new_id <- function(prefix) {
  paste0(prefix, "_", format(Sys.time(), "%H%M%OS3"), "_", paste(sample(letters, 4), collapse = ""))
}

# ---------------------------------------------------------------------------
# Data Intake tools
# ---------------------------------------------------------------------------

register_tool(list(
  name = "inspect_uploaded_file",
  description = "Inspect an already-uploaded, validated file: extension, size, checksum, and detected format. Does not parse spectral content.",
  input_schema = list(
    type = "object",
    properties = list(file_id = list(type = "string", description = "Session file id of a previously uploaded file")),
    required = list("file_id")
  ),
  validate = function(args, ctx) list(ok = TRUE, errors = character(0), args = args),
  execute = function(args, ctx) {
    rec <- ctx$get_upload(args$file_id)
    if (is.null(rec)) stop("Unknown file_id.")
    info <- inspect_uploaded_file(rec$path, rec$original_filename)
    list(output = info, output_id = NA_character_, artifacts = character(0), warnings = character(0))
  },
  output_schema = list(type = "object"),
  timeout_s = 15,
  expected_artifacts = character(0)
))

register_tool(list(
  name = "import_ms_file",
  description = "Import a validated mzML/mzXML/MGF file into a Spectra object and wrap it in an MsExperiment.",
  input_schema = list(
    type = "object",
    properties = list(file_id = list(type = "string")),
    required = list("file_id")
  ),
  validate = function(args, ctx) list(ok = TRUE, errors = character(0), args = args),
  execute = function(args, ctx) {
    rec <- ctx$get_upload(args$file_id)
    if (is.null(rec)) stop("Unknown file_id.")
    res <- import_ms_file(rec$path, rec$original_filename)
    sp_id <- .new_id("spectra")
    exp_id <- .new_id("msexperiment")
    provenance_put_object(ctx$store, sp_id, res$spectra)
    provenance_put_object(ctx$store, exp_id, res$ms_experiment)
    list(output = list(meta = res$meta, spectra_id = sp_id, ms_experiment_id = exp_id),
         output_id = sp_id, artifacts = character(0), warnings = character(0))
  },
  output_schema = list(type = "object"),
  timeout_s = 120,
  expected_artifacts = character(0)
))

register_tool(list(
  name = "summarize_experiment",
  description = "Summarize an imported Spectra/MsExperiment: spectrum counts by MS level, RT range, polarity.",
  input_schema = list(
    type = "object",
    properties = list(spectra_id = list(type = "string")),
    required = list("spectra_id")
  ),
  validate = function(args, ctx) list(ok = TRUE, errors = character(0), args = args),
  execute = function(args, ctx) {
    sp <- .resolve_object(ctx, args$spectra_id, "Spectra")
    out <- summarize_experiment(sp)
    list(output = out, output_id = NA_character_, artifacts = character(0), warnings = character(0))
  },
  output_schema = list(type = "object"),
  timeout_s = 30,
  expected_artifacts = character(0)
))

register_tool(list(
  name = "list_spectra",
  description = "List a page of spectra with index, MS level, retention time, precursor m/z, and peak count.",
  input_schema = list(
    type = "object",
    properties = list(
      spectra_id = list(type = "string"),
      offset = list(type = "integer", minimum = 0),
      limit  = list(type = "integer", minimum = 1, maximum = 500)
    ),
    required = list("spectra_id")
  ),
  validate = function(args, ctx) {
    errors <- character(0)
    if (!is.null(args$limit) && (args$limit < 1 || args$limit > 500)) errors <- c(errors, "limit must be between 1 and 500.")
    args$offset <- args$offset %||% 0L
    args$limit <- args$limit %||% 50L
    list(ok = length(errors) == 0, errors = errors, args = args)
  },
  execute = function(args, ctx) {
    sp <- .resolve_object(ctx, args$spectra_id, "Spectra")
    n <- length(sp)
    idx <- seq(args$offset + 1, min(args$offset + args$limit, n))
    if (length(idx) == 0) {
      return(list(output = list(rows = list(), n_total = n), output_id = NA_character_,
                   artifacts = character(0), warnings = character(0)))
    }
    sub <- sp[idx]
    df <- data.frame(
      index = idx,
      ms_level = Spectra::msLevel(sub),
      rtime_s = Spectra::rtime(sub),
      precursor_mz = tryCatch(Spectra::precursorMz(sub), error = function(e) NA_real_),
      n_peaks = lengths(sub)
    )
    list(output = list(rows = df, n_total = n), output_id = NA_character_, artifacts = character(0), warnings = character(0))
  },
  output_schema = list(type = "object"),
  timeout_s = 30,
  expected_artifacts = character(0)
))

register_tool(list(
  name = "get_spectrum",
  description = "Retrieve peak data (m/z, intensity) and metadata for a single spectrum by index.",
  input_schema = list(
    type = "object",
    properties = list(spectra_id = list(type = "string"), index = list(type = "integer", minimum = 1)),
    required = list("spectra_id", "index")
  ),
  validate = function(args, ctx) {
    errors <- character(0)
    if (is.null(args$index) || args$index < 1) errors <- c(errors, "index must be >= 1.")
    list(ok = length(errors) == 0, errors = errors, args = args)
  },
  execute = function(args, ctx) {
    sp <- .resolve_object(ctx, args$spectra_id, "Spectra")
    if (args$index > length(sp)) stop("index exceeds number of spectra.")
    one <- sp[args$index]
    pk <- Spectra::peaksData(one)[[1]]
    list(output = list(
      index = args$index,
      ms_level = Spectra::msLevel(one),
      rtime_s = Spectra::rtime(one),
      precursor_mz = tryCatch(Spectra::precursorMz(one), error = function(e) NA_real_),
      n_peaks = nrow(pk),
      peaks_preview = utils::head(as.data.frame(pk), 50)
    ), output_id = NA_character_, artifacts = character(0), warnings = character(0))
  },
  output_schema = list(type = "object"),
  timeout_s = 15,
  expected_artifacts = character(0)
))

# ---------------------------------------------------------------------------
# QC tools
# ---------------------------------------------------------------------------

register_tool(list(
  name = "calculate_qc_metrics",
  description = "Calculate file- and spectrum-level QC metrics with severity-tagged warnings.",
  input_schema = list(
    type = "object",
    properties = list(spectra_id = list(type = "string")),
    required = list("spectra_id")
  ),
  validate = function(args, ctx) list(ok = TRUE, errors = character(0), args = args),
  execute = function(args, ctx) {
    sp <- .resolve_object(ctx, args$spectra_id, "Spectra")
    m <- calculate_qc_metrics(sp)
    list(output = m[setdiff(names(m), "warnings")], output_id = NA_character_,
         artifacts = character(0), warnings = m$warnings)
  },
  output_schema = list(type = "object"),
  timeout_s = 60,
  expected_artifacts = character(0)
))

.plot_tool <- function(name, description, plot_fn, extra_props = list(), extra_required = list()) {
  register_tool(list(
    name = name,
    description = description,
    input_schema = list(
      type = "object",
      properties = c(list(spectra_id = list(type = "string")), extra_props),
      required = c(list("spectra_id"), extra_required)
    ),
    validate = function(args, ctx) list(ok = TRUE, errors = character(0), args = args),
    execute = function(args, ctx) {
      sp <- .resolve_object(ctx, args$spectra_id, "Spectra")
      p <- do.call(plot_fn, c(list(sp = sp), args[setdiff(names(args), "spectra_id")]))
      art_id <- .new_id(paste0("plot_", name))
      path <- file.path(ctx$session_dir, paste0(art_id, ".png"))
      ggplot2::ggsave(path, plot = p, width = 8, height = 4.5, dpi = 120)
      list(output = list(artifact_path = path), output_id = NA_character_,
           artifacts = path, warnings = character(0))
    },
    output_schema = list(type = "object"),
    timeout_s = 30,
    expected_artifacts = "png"
  ))
}

.plot_tool("plot_tic", "Generate a total ion chromatogram plot for a given MS level.",
           plot_tic, extra_props = list(ms_level = list(type = "integer", minimum = 1, maximum = 2)))
.plot_tool("plot_bpc", "Generate a base peak chromatogram plot for a given MS level.",
           plot_bpc, extra_props = list(ms_level = list(type = "integer", minimum = 1, maximum = 2)))
.plot_tool("plot_spectrum", "Generate a stick plot for a single spectrum by index.",
           plot_spectrum, extra_props = list(index = list(type = "integer", minimum = 1)),
           extra_required = list("index"))

# ---------------------------------------------------------------------------
# Spectrum processing tools
# ---------------------------------------------------------------------------

register_tool(list(
  name = "filter_spectra",
  description = "Filter spectra by MS level, retention time, precursor m/z, m/z range, and/or intensity. Produces a new named processing stage; the input is never modified.",
  input_schema = list(
    type = "object",
    properties = list(
      spectra_id = list(type = "string"),
      stage_name = list(type = "string", description = "Name for the resulting processing stage, e.g. 'ms2_filtered'"),
      ms_level = list(type = "array", items = list(type = "integer")),
      rt_min = list(type = "number"), rt_max = list(type = "number"),
      precursor_mz_min = list(type = "number"), precursor_mz_max = list(type = "number"),
      mz_min = list(type = "number"), mz_max = list(type = "number"),
      min_intensity = list(type = "number", minimum = 0),
      min_relative_intensity = list(type = "number", minimum = 0, maximum = 1)
    ),
    required = list("spectra_id", "stage_name")
  ),
  validate = function(args, ctx) {
    errors <- character(0)
    if (!is.null(args$rt_min) && !is.null(args$rt_max) && args$rt_min > args$rt_max) errors <- c(errors, "rt_min must be <= rt_max.")
    if (!is.null(args$mz_min) && !is.null(args$mz_max) && args$mz_min > args$mz_max) errors <- c(errors, "mz_min must be <= mz_max.")
    if (!is.null(args$min_relative_intensity) && (args$min_relative_intensity < 0 || args$min_relative_intensity > 1)) {
      errors <- c(errors, "min_relative_intensity must be between 0 and 1.")
    }
    list(ok = length(errors) == 0, errors = errors, args = args)
  },
  execute = function(args, ctx) {
    sp <- .resolve_object(ctx, args$spectra_id, "Spectra")
    out <- filter_spectra(sp,
      ms_level = args$ms_level, rt_min = args$rt_min, rt_max = args$rt_max,
      precursor_mz_min = args$precursor_mz_min, precursor_mz_max = args$precursor_mz_max,
      mz_min = args$mz_min, mz_max = args$mz_max,
      min_intensity = args$min_intensity, min_relative_intensity = args$min_relative_intensity)
    out_id <- paste0("spectra_", safe_filename(args$stage_name))
    provenance_put_object(ctx$store, out_id, out)
    cmp <- compare_spectra(sp, out, "input", args$stage_name)
    list(output = list(stage_id = out_id, n_before = length(sp), n_after = length(out), comparison = cmp),
         output_id = out_id, artifacts = character(0), warnings = character(0))
  },
  output_schema = list(type = "object"),
  timeout_s = 60,
  expected_artifacts = character(0)
))

register_tool(list(
  name = "normalize_spectra",
  description = "Normalize spectrum intensities (tic or max) into a new named processing stage.",
  input_schema = list(
    type = "object",
    properties = list(
      spectra_id = list(type = "string"),
      stage_name = list(type = "string"),
      method = list(type = "string", enum = list("tic", "max"))
    ),
    required = list("spectra_id", "stage_name", "method")
  ),
  validate = function(args, ctx) {
    errors <- character(0)
    if (!args$method %in% c("tic", "max")) errors <- c(errors, "method must be 'tic' or 'max'.")
    list(ok = length(errors) == 0, errors = errors, args = args)
  },
  execute = function(args, ctx) {
    sp <- .resolve_object(ctx, args$spectra_id, "Spectra")
    out <- normalize_spectra(sp, method = args$method)
    out_id <- paste0("spectra_", safe_filename(args$stage_name))
    provenance_put_object(ctx$store, out_id, out)
    list(output = list(stage_id = out_id, method = args$method, n_spectra = length(out)),
         output_id = out_id, artifacts = character(0), warnings = character(0))
  },
  output_schema = list(type = "object"),
  timeout_s = 60,
  expected_artifacts = character(0)
))

register_tool(list(
  name = "retain_top_peaks",
  description = "Retain only the top-N most intense peaks per spectrum into a new named processing stage.",
  input_schema = list(
    type = "object",
    properties = list(
      spectra_id = list(type = "string"),
      stage_name = list(type = "string"),
      n = list(type = "integer", minimum = 1, maximum = 2000)
    ),
    required = list("spectra_id", "stage_name", "n")
  ),
  validate = function(args, ctx) {
    errors <- character(0)
    if (args$n < 1 || args$n > 2000) errors <- c(errors, "n must be between 1 and 2000.")
    list(ok = length(errors) == 0, errors = errors, args = args)
  },
  execute = function(args, ctx) {
    sp <- .resolve_object(ctx, args$spectra_id, "Spectra")
    out <- retain_top_peaks(sp, n = args$n)
    out_id <- paste0("spectra_", safe_filename(args$stage_name))
    provenance_put_object(ctx$store, out_id, out)
    cmp <- compare_spectra(sp, out, "input", args$stage_name)
    list(output = list(stage_id = out_id, n_peaks_retained = args$n, comparison = cmp),
         output_id = out_id, artifacts = character(0), warnings = character(0))
  },
  output_schema = list(type = "object"),
  timeout_s = 60,
  expected_artifacts = character(0)
))

register_tool(list(
  name = "compare_spectra",
  description = "Compare aggregate metrics between two spectra processing stages (before vs after).",
  input_schema = list(
    type = "object",
    properties = list(spectra_id_before = list(type = "string"), spectra_id_after = list(type = "string")),
    required = list("spectra_id_before", "spectra_id_after")
  ),
  validate = function(args, ctx) list(ok = TRUE, errors = character(0), args = args),
  execute = function(args, ctx) {
    b <- .resolve_object(ctx, args$spectra_id_before, "Spectra")
    a <- .resolve_object(ctx, args$spectra_id_after, "Spectra")
    cmp <- compare_spectra(b, a)
    list(output = cmp, output_id = NA_character_, artifacts = character(0), warnings = character(0))
  },
  output_schema = list(type = "object"),
  timeout_s = 30,
  expected_artifacts = character(0)
))

# ---------------------------------------------------------------------------
# Identification tools
# ---------------------------------------------------------------------------

register_tool(list(
  name = "import_psm_table",
  description = "Import a validated PSM identification CSV/TSV table with delimiter auto-detection and column mapping.",
  input_schema = list(
    type = "object",
    properties = list(file_id = list(type = "string")),
    required = list("file_id")
  ),
  validate = function(args, ctx) list(ok = TRUE, errors = character(0), args = args),
  execute = function(args, ctx) {
    rec <- ctx$get_upload(args$file_id)
    if (is.null(rec)) stop("Unknown file_id.")
    df <- import_psm_table(rec$path)
    val <- validate_psm_table(df)
    if (!val$ok) stop(paste(val$errors, collapse = " "))
    psm_id <- .new_id("psm")
    provenance_put_object(ctx$store, psm_id, val$mapped_df)
    list(output = list(psm_id = psm_id, n_rows = val$n_rows, column_map = val$column_map,
                        delimiter = attr(df, "detected_delimiter")),
         output_id = psm_id, artifacts = character(0), warnings = character(0))
  },
  output_schema = list(type = "object"),
  timeout_s = 30,
  expected_artifacts = character(0)
))

register_tool(list(
  name = "filter_psms",
  description = "Filter a PSM table by minimum score and/or maximum q-value/FDR.",
  input_schema = list(
    type = "object",
    properties = list(
      psm_id = list(type = "string"),
      stage_name = list(type = "string"),
      min_score = list(type = "number"),
      max_qvalue = list(type = "number", minimum = 0, maximum = 1)
    ),
    required = list("psm_id", "stage_name")
  ),
  validate = function(args, ctx) {
    errors <- character(0)
    if (!is.null(args$max_qvalue) && (args$max_qvalue < 0 || args$max_qvalue > 1)) errors <- c(errors, "max_qvalue must be in [0,1].")
    list(ok = length(errors) == 0, errors = errors, args = args)
  },
  execute = function(args, ctx) {
    df <- .resolve_object(ctx, args$psm_id, "data.frame")
    out <- filter_psms(df, min_score = args$min_score, max_qvalue = args$max_qvalue)
    out_id <- paste0("psm_", safe_filename(args$stage_name))
    provenance_put_object(ctx$store, out_id, out)
    list(output = list(stage_id = out_id, n_before = nrow(df), n_after = nrow(out)),
         output_id = out_id, artifacts = character(0), warnings = character(0))
  },
  output_schema = list(type = "object"),
  timeout_s = 30,
  expected_artifacts = character(0)
))

register_tool(list(
  name = "summarize_identifications",
  description = "Summarize PSM, peptide, and protein counts from a (filtered) PSM table.",
  input_schema = list(
    type = "object",
    properties = list(psm_id = list(type = "string")),
    required = list("psm_id")
  ),
  validate = function(args, ctx) list(ok = TRUE, errors = character(0), args = args),
  execute = function(args, ctx) {
    df <- .resolve_object(ctx, args$psm_id, "data.frame")
    s <- summarize_identifications(df)
    list(output = s[c("n_psm", "n_unique_peptides", "n_unique_proteins", "median_psms_per_protein")],
         output_id = NA_character_, artifacts = character(0), warnings = character(0))
  },
  output_schema = list(type = "object"),
  timeout_s = 30,
  expected_artifacts = character(0)
))

# ---------------------------------------------------------------------------
# Quantification tools
# ---------------------------------------------------------------------------

register_tool(list(
  name = "import_quant_table",
  description = "Import a validated peptide/protein abundance table (CSV/TSV).",
  input_schema = list(
    type = "object",
    properties = list(file_id = list(type = "string"), id_col = list(type = "string")),
    required = list("file_id")
  ),
  validate = function(args, ctx) list(ok = TRUE, errors = character(0), args = args),
  execute = function(args, ctx) {
    rec <- ctx$get_upload(args$file_id)
    if (is.null(rec)) stop("Unknown file_id.")
    df <- import_quant_table(rec$path)
    val <- validate_quant_table(df, id_col = args$id_col)
    if (!val$ok) stop(paste(val$errors, collapse = " "))
    tbl_id <- .new_id("quanttable")
    provenance_put_object(ctx$store, tbl_id, df)
    list(output = list(table_id = tbl_id, id_col = val$id_col, sample_cols = val$sample_cols, n_rows = nrow(df)),
         output_id = tbl_id, artifacts = character(0), warnings = character(0))
  },
  output_schema = list(type = "object"),
  timeout_s = 30,
  expected_artifacts = character(0)
))

register_tool(list(
  name = "build_qfeatures",
  description = "Build a QFeatures object from an imported abundance table.",
  input_schema = list(
    type = "object",
    properties = list(
      table_id = list(type = "string"),
      id_col = list(type = "string"),
      sample_cols = list(type = "array", items = list(type = "string")),
      assay_name = list(type = "string")
    ),
    required = list("table_id", "id_col", "sample_cols", "assay_name")
  ),
  validate = function(args, ctx) list(ok = TRUE, errors = character(0), args = args),
  execute = function(args, ctx) {
    df <- .resolve_object(ctx, args$table_id, "data.frame")
    qf <- build_qfeatures(df, args$id_col, unlist(args$sample_cols), assay_name = args$assay_name)
    qf_id <- .new_id("qfeatures")
    provenance_put_object(ctx$store, qf_id, qf)
    list(output = list(qfeatures_id = qf_id, assay_name = args$assay_name, dims = dim(qf[[args$assay_name]])),
         output_id = qf_id, artifacts = character(0), warnings = character(0))
  },
  output_schema = list(type = "object"),
  timeout_s = 60,
  expected_artifacts = character(0)
))

register_tool(list(
  name = "normalize_quantification",
  description = "Normalize a QFeatures assay (default: median centering) into a new assay.",
  input_schema = list(
    type = "object",
    properties = list(qfeatures_id = list(type = "string"), assay_name = list(type = "string"),
                       method = list(type = "string")),
    required = list("qfeatures_id", "assay_name")
  ),
  validate = function(args, ctx) list(ok = TRUE, errors = character(0), args = args),
  execute = function(args, ctx) {
    qf <- .resolve_object(ctx, args$qfeatures_id, "QFeatures")
    method <- args$method %||% "center.median"
    qf2 <- normalize_quantification(qf, args$assay_name, method = method)
    qf_id <- .new_id("qfeatures")
    provenance_put_object(ctx$store, qf_id, qf2)
    new_assay <- paste0(args$assay_name, "_norm")
    list(output = list(qfeatures_id = qf_id, new_assay = new_assay, method = method),
         output_id = qf_id, artifacts = character(0), warnings = character(0))
  },
  output_schema = list(type = "object"),
  timeout_s = 60,
  expected_artifacts = character(0)
))

register_tool(list(
  name = "aggregate_to_proteins",
  description = "Aggregate a peptide-level QFeatures assay to protein level using a protein id column.",
  input_schema = list(
    type = "object",
    properties = list(qfeatures_id = list(type = "string"), assay_name = list(type = "string"),
                       fcol = list(type = "string")),
    required = list("qfeatures_id", "assay_name")
  ),
  validate = function(args, ctx) list(ok = TRUE, errors = character(0), args = args),
  execute = function(args, ctx) {
    qf <- .resolve_object(ctx, args$qfeatures_id, "QFeatures")
    fcol <- args$fcol %||% "protein"
    qf2 <- aggregate_to_proteins(qf, args$assay_name, fcol = fcol)
    qf_id <- .new_id("qfeatures")
    provenance_put_object(ctx$store, qf_id, qf2)
    new_assay <- paste0(args$assay_name, "_proteins")
    list(output = list(qfeatures_id = qf_id, new_assay = new_assay,
                        dims = dim(qf2[[new_assay]])),
         output_id = qf_id, artifacts = character(0), warnings = character(0))
  },
  output_schema = list(type = "object"),
  timeout_s = 60,
  expected_artifacts = character(0)
))

register_tool(list(
  name = "run_exploratory_analysis",
  description = "Run PCA (and, when statistically valid, a simple two-group comparison) on a QFeatures assay.",
  input_schema = list(
    type = "object",
    properties = list(
      qfeatures_id = list(type = "string"), assay_name = list(type = "string"),
      group_column = list(type = "string")
    ),
    required = list("qfeatures_id", "assay_name")
  ),
  validate = function(args, ctx) list(ok = TRUE, errors = character(0), args = args),
  execute = function(args, ctx) {
    qf <- .resolve_object(ctx, args$qfeatures_id, "QFeatures")
    pca <- run_pca(qf, args$assay_name)
    comparison <- NULL
    if (pca$ok && !is.null(args$group_column)) {
      cd <- SummarizedExperiment::colData(qf[[args$assay_name]])
      if (args$group_column %in% colnames(cd)) {
        comparison <- run_two_group_comparison(qf, args$assay_name, cd[[args$group_column]])
      }
    }
    warn <- character(0)
    if (!pca$ok) warn <- c(warn, pca$reason)
    list(output = list(pca = pca, comparison = comparison), output_id = NA_character_,
         artifacts = character(0), warnings = warn)
  },
  output_schema = list(type = "object"),
  timeout_s = 60,
  expected_artifacts = character(0)
))

# ---------------------------------------------------------------------------
# Reporting tool
# ---------------------------------------------------------------------------

register_tool(list(
  name = "generate_report",
  description = "Generate the final reproducible HTML analysis report from all computed results and the agent trace.",
  input_schema = list(
    type = "object",
    properties = list(objective = list(type = "string")),
    required = list("objective")
  ),
  validate = function(args, ctx) list(ok = TRUE, errors = character(0), args = args),
  execute = function(args, ctx) {
    report_params <- ctx$build_report_params(args$objective)
    path <- generate_report(report_params, ctx$session_dir)
    list(output = list(report_path = path), output_id = NA_character_,
         artifacts = path, warnings = character(0))
  },
  output_schema = list(type = "object"),
  timeout_s = 180,
  expected_artifacts = "html"
))
