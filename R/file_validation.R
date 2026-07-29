# R/file_validation.R
#
# Upload safety: extension allowlist, size limits, filename sanitization,
# path-traversal protection, checksums, and per-session temp directories.
# This module is a hard security boundary used by both the Shiny upload
# handlers and the Data Intake agent tool `import_ms_file`.

ALLOWED_MS_EXTENSIONS <- c("mzml", "mzxml", "mgf")
ALLOWED_TABLE_EXTENSIONS <- c("csv", "tsv", "txt")
ALLOWED_FASTA_EXTENSIONS <- c("fasta", "fa", "fas")

#' Extract a lowercased, dot-free file extension.
#' @export
file_extension <- function(filename) {
  parts <- strsplit(basename(filename), "\\.")[[1]]
  if (length(parts) < 2) return("")
  tolower(parts[length(parts)])
}

#' Sanitize a user-supplied filename: strip directories, forbid traversal,
#' allow only a conservative character set.
#'
#' @export
safe_filename <- function(filename) {
  base <- basename(filename)
  base <- gsub("\\.\\.", "", base)
  base <- gsub("[^A-Za-z0-9._-]", "_", base)
  if (!nzchar(base) || base %in% c(".", "..")) {
    stop("Invalid filename after sanitization.")
  }
  base
}

#' Validate that a resolved path stays within an allowed base directory
#' (defense against path traversal even if a filename slipped through).
#'
#' @export
assert_within_dir <- function(path, base_dir) {
  base_norm <- normalizePath(base_dir, mustWork = TRUE, winslash = "/")
  target_norm <- suppressWarnings(normalizePath(path, mustWork = FALSE, winslash = "/"))
  if (!startsWith(target_norm, base_norm)) {
    stop("Path traversal detected: resolved path escapes the allowed directory.")
  }
  invisible(TRUE)
}

#' Create a fresh, private, per-session temp directory under a root temp dir.
#' @export
create_session_dir <- function(root = tempdir(), session_id = NULL) {
  session_id <- session_id %||% paste0("s", format(Sys.time(), "%Y%m%d%H%M%S"), "-",
                                        paste(sample(c(letters, 0:9), 8, TRUE), collapse = ""))
  dir_path <- file.path(root, paste0("bpa_", session_id))
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(dir_path, winslash = "/")
}

#' Remove a session temp directory and all of its contents.
#' @export
cleanup_session_dir <- function(dir_path) {
  if (nzchar(dir_path) && dir.exists(dir_path) && grepl("bpa_", basename(dir_path), fixed = TRUE)) {
    unlink(dir_path, recursive = TRUE, force = TRUE)
  }
  invisible(TRUE)
}

#' Validate an uploaded MS file: extension allowlist + size limit + safe name.
#'
#' @param filename original filename as supplied by the browser
#' @param filesize size in bytes
#' @param max_upload_mb configured maximum upload size in MB
#' @return list(ok = logical, reason = character, extension = character, safe_name = character)
#' @export
validate_ms_upload <- function(filename, filesize, max_upload_mb = 500) {
  ext <- file_extension(filename)
  if (!ext %in% ALLOWED_MS_EXTENSIONS) {
    return(list(ok = FALSE,
                reason = sprintf("Unsupported file extension '.%s'. Allowed: %s.",
                                  ext, paste(ALLOWED_MS_EXTENSIONS, collapse = ", ")),
                extension = ext, safe_name = NA_character_))
  }
  max_bytes <- max_upload_mb * 1024 * 1024
  if (is.na(filesize) || filesize <= 0) {
    return(list(ok = FALSE, reason = "Empty or unreadable upload.", extension = ext, safe_name = NA_character_))
  }
  if (filesize > max_bytes) {
    return(list(ok = FALSE,
                reason = sprintf("File exceeds the maximum upload size of %.0f MB.", max_upload_mb),
                extension = ext, safe_name = NA_character_))
  }
  safe_name <- tryCatch(safe_filename(filename), error = function(e) NA_character_)
  if (is.na(safe_name)) {
    return(list(ok = FALSE, reason = "Filename could not be safely sanitized.", extension = ext, safe_name = NA_character_))
  }
  list(ok = TRUE, reason = "", extension = ext, safe_name = safe_name)
}

#' Validate an uploaded PSM/quant table upload (csv/tsv/txt only).
#' @export
validate_table_upload <- function(filename, filesize, max_upload_mb = 500) {
  ext <- file_extension(filename)
  if (!ext %in% ALLOWED_TABLE_EXTENSIONS) {
    return(list(ok = FALSE,
                reason = sprintf("Unsupported table extension '.%s'. Allowed: %s.",
                                  ext, paste(ALLOWED_TABLE_EXTENSIONS, collapse = ", ")),
                extension = ext, safe_name = NA_character_))
  }
  max_bytes <- max_upload_mb * 1024 * 1024
  if (is.na(filesize) || filesize <= 0) {
    return(list(ok = FALSE, reason = "Empty or unreadable upload.", extension = ext, safe_name = NA_character_))
  }
  if (filesize > max_bytes) {
    return(list(ok = FALSE,
                reason = sprintf("File exceeds the maximum upload size of %.0f MB.", max_upload_mb),
                extension = ext, safe_name = NA_character_))
  }
  safe_name <- tryCatch(safe_filename(filename), error = function(e) NA_character_)
  if (is.na(safe_name)) {
    return(list(ok = FALSE, reason = "Filename could not be safely sanitized.", extension = ext, safe_name = NA_character_))
  }
  list(ok = TRUE, reason = "", extension = ext, safe_name = safe_name)
}

#' Validate an uploaded FASTA protein database (fasta/fa/fas only).
#' @export
validate_fasta_upload <- function(filename, filesize, max_upload_mb = 500) {
  ext <- file_extension(filename)
  if (!ext %in% ALLOWED_FASTA_EXTENSIONS) {
    return(list(ok = FALSE,
                reason = sprintf("Unsupported FASTA extension '.%s'. Allowed: %s.",
                                  ext, paste(ALLOWED_FASTA_EXTENSIONS, collapse = ", ")),
                extension = ext, safe_name = NA_character_))
  }
  max_bytes <- max_upload_mb * 1024 * 1024
  if (is.na(filesize) || filesize <= 0) {
    return(list(ok = FALSE, reason = "Empty or unreadable upload.", extension = ext, safe_name = NA_character_))
  }
  if (filesize > max_bytes) {
    return(list(ok = FALSE,
                reason = sprintf("File exceeds the maximum upload size of %.0f MB.", max_upload_mb),
                extension = ext, safe_name = NA_character_))
  }
  safe_name <- tryCatch(safe_filename(filename), error = function(e) NA_character_)
  if (is.na(safe_name)) {
    return(list(ok = FALSE, reason = "Filename could not be safely sanitized.", extension = ext, safe_name = NA_character_))
  }
  list(ok = TRUE, reason = "", extension = ext, safe_name = safe_name)
}

#' Compute a SHA-256 checksum of a file for provenance / duplicate detection.
#' @export
file_checksum <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("The 'digest' package is required for checksums.")
  }
  digest::digest(file = path, algo = "sha256")
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
