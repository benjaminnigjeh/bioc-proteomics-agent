# tests/testthat.R
#
# Test runner. Run from the repository root:
#   LLM_MODE=mock Rscript -e "testthat::test_dir('tests/testthat')"
# or:
#   make test

Sys.setenv(LLM_MODE = Sys.getenv("LLM_MODE", unset = "mock"))

testthat::test_dir("tests/testthat")
