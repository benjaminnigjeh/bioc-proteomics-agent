#!/usr/bin/env Rscript
# scripts/install_packages.R
#
# Installs pinned CRAN and Bioconductor dependencies for local
# (non-Docker) development. The Docker image installs the same set of
# packages at build time (see Dockerfile) -- keep the two lists in sync.

bioc_version <- "3.19"

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager", repos = "https://cloud.r-project.org")
}
BiocManager::install(version = bioc_version, ask = FALSE, update = FALSE)

cran_pkgs <- c(
  "shiny", "bslib", "httr2", "jsonlite", "digest", "ggplot2",
  "rmarkdown", "knitr", "htmltools", "testthat", "shinytest2",
  "R.utils", "later", "DT", "withr"
)

bioc_pkgs <- c(
  "Spectra", "MsExperiment", "MsCoreUtils", "mzR", "ProtGenerics",
  "QFeatures", "SummarizedExperiment", "MultiAssayExperiment", "BiocParallel",
  "MsBackendMzR", "MsBackendMgf", "PSMatch", "limma"
)

to_install_cran <- cran_pkgs[!vapply(cran_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install_cran) > 0) {
  BiocManager::install(to_install_cran, ask = FALSE, update = FALSE)
}

to_install_bioc <- bioc_pkgs[!vapply(bioc_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install_bioc) > 0) {
  BiocManager::install(to_install_bioc, ask = FALSE, update = FALSE)
}

cat("All required packages are installed.\n")
cat("Next: Rscript scripts/generate_demo_data.R && Rscript scripts/run_app.R\n")
