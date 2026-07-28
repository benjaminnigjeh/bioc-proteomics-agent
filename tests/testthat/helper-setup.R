# tests/testthat/helper-setup.R
#
# testthat automatically sources files named helper-*.R before running
# tests. This project has no package DESCRIPTION, so R/ and modules/ are
# sourced manually here. Tests must be run with the working directory set
# to the repository root (this is how the Makefile, tests/testthat.R, and
# CI all invoke them).

Sys.setenv(LLM_MODE = Sys.getenv("LLM_MODE", unset = "mock"))

.test_root <- if (basename(getwd()) == "testthat" ) file.path("..", "..") else "."

for (f in list.files(file.path(.test_root, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = FALSE)
}

#' Build a small synthetic Spectra object for unit tests, without any file
#' I/O. 3 MS1 scans and 3 MS2 scans, deterministic peaks.
make_test_spectra <- function() {
  spd <- S4Vectors::DataFrame(
    msLevel = c(1L, 2L, 1L, 2L, 1L, 2L),
    rtime = c(1.0, 1.5, 2.0, 2.5, 3.0, 3.5),
    polarity = rep(1L, 6),
    precursorMz = c(NA, 500.25, NA, 600.30, NA, 700.10),
    precursorCharge = c(NA, 2L, NA, 2L, NA, 3L),
    centroided = TRUE
  )
  spd$mz <- list(
    c(100.1, 200.2, 300.3),
    c(110.5, 220.6, 330.7, 440.8),
    c(150.1, 250.2),
    c(120.0, 230.0, 340.0),
    numeric(0),
    c(101.1, 202.2, 303.3, 404.4, 505.5)
  )
  spd$intensity <- list(
    c(1000, 2000, 500),
    c(300, 900, 150, 50),
    c(800, 400),
    c(500, 700, 100),
    numeric(0),
    c(200, 800, 600, 300, 100)
  )
  Spectra::Spectra(spd)
}

#' Build a minimal, valid provenance execution context for guardrail/tool
#' tests, without needing a real Shiny session.
make_test_ctx <- function(store = NULL) {
  store <- store %||% new_provenance_store()
  dir <- tempfile("bpa_test_")
  dir.create(dir)
  uploads <- new.env()
  new_session_ctx(store, dir, uploads, report_params_builder = function(objective) list(objective = objective))
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
