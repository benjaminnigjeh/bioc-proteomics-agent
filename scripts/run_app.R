#!/usr/bin/env Rscript
# scripts/run_app.R
#
# Runs the Shiny app locally without Docker, loading .env for development
# convenience (never required in the Docker image, where env vars come
# from `env_file:` in docker-compose.yml instead).

if (file.exists(".env") && requireNamespace("dotenv", quietly = TRUE)) {
  dotenv::load_dot_env(".env")
} else if (file.exists(".env")) {
  # Minimal .env loader if the optional 'dotenv' package isn't installed.
  lines <- readLines(".env", warn = FALSE)
  lines <- lines[nzchar(trimws(lines)) & !startsWith(trimws(lines), "#")]
  for (line in lines) {
    kv <- regmatches(line, regexpr("=", line), invert = NA)
    parts <- strsplit(line, "=", fixed = TRUE)[[1]]
    key <- trimws(parts[1])
    value <- trimws(paste(parts[-1], collapse = "="))
    if (nzchar(key)) do.call(Sys.setenv, stats::setNames(list(value), key))
  }
}

port <- suppressWarnings(as.integer(Sys.getenv("SHINY_PORT", unset = "3838")))
if (is.na(port)) port <- 3838

cat(sprintf("Starting Bioconductor Proteomics Agent on http://localhost:%d (LLM_MODE=%s)\n",
            port, Sys.getenv("LLM_MODE", unset = "mock")))

shiny::runApp(".", host = "0.0.0.0", port = port, launch.browser = FALSE)
