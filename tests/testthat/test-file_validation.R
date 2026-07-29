test_that("supported MS file extensions are detected correctly", {
  expect_equal(file_extension("sample.mzML"), "mzml")
  expect_equal(file_extension("sample.MZXML"), "mzxml")
  expect_equal(file_extension("sample.mgf"), "mgf")
  expect_equal(file_extension("sample.raw"), "raw")
})

test_that("validate_ms_upload accepts allowed extensions and rejects others", {
  ok <- validate_ms_upload("demo.mzML", filesize = 1000, max_upload_mb = 500)
  expect_true(ok$ok)

  bad <- validate_ms_upload("demo.raw", filesize = 1000, max_upload_mb = 500)
  expect_false(bad$ok)
  expect_match(bad$reason, "Unsupported")
})

test_that("upload size validation rejects oversized files", {
  too_big <- validate_ms_upload("demo.mzML", filesize = 600 * 1024 * 1024, max_upload_mb = 500)
  expect_false(too_big$ok)
  expect_match(too_big$reason, "exceeds")

  empty <- validate_ms_upload("demo.mzML", filesize = 0, max_upload_mb = 500)
  expect_false(empty$ok)
})

test_that("unsafe filenames are rejected or sanitized", {
  # basename() strips all directory components (including "../"), so the
  # sanitized result is a bare filename with no traversal potential left.
  traversal_result <- safe_filename("../../etc/passwd")
  expect_false(grepl("/", traversal_result, fixed = TRUE))
  expect_false(grepl("..", traversal_result, fixed = TRUE))
  expect_equal(traversal_result, "passwd")

  expect_equal(safe_filename("plain_name.mzML"), "plain_name.mzML")
  expect_equal(safe_filename("weird name!@#.mzML"), "weird_name___.mzML")
})

test_that("path traversal outside the session directory is detected", {
  base <- tempfile("bpa_base_")
  dir.create(base)
  on.exit(unlink(base, recursive = TRUE))
  inside <- file.path(base, "ok.txt")
  file.create(inside)
  expect_true(assert_within_dir(inside, base))

  outside <- file.path(dirname(base), "outside.txt")
  file.create(outside)
  on.exit(unlink(outside), add = TRUE)
  expect_error(assert_within_dir(outside, base))
})

test_that("validate_fasta_upload accepts allowed extensions and rejects others", {
  ok <- validate_fasta_upload("demo.fasta", filesize = 1000, max_upload_mb = 500)
  expect_true(ok$ok)
  ok2 <- validate_fasta_upload("demo.fa", filesize = 1000, max_upload_mb = 500)
  expect_true(ok2$ok)

  bad <- validate_fasta_upload("demo.csv", filesize = 1000, max_upload_mb = 500)
  expect_false(bad$ok)
  expect_match(bad$reason, "Unsupported")

  too_big <- validate_fasta_upload("demo.fasta", filesize = 600 * 1024 * 1024, max_upload_mb = 500)
  expect_false(too_big$ok)
  expect_match(too_big$reason, "exceeds")

  empty <- validate_fasta_upload("demo.fasta", filesize = 0, max_upload_mb = 500)
  expect_false(empty$ok)
})

test_that("file checksums are deterministic", {
  f <- tempfile()
  writeLines("hello world", f)
  on.exit(unlink(f))
  expect_equal(file_checksum(f), file_checksum(f))
  expect_equal(nchar(file_checksum(f)), 64)
})
