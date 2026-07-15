valid_cl_obo <- c(
  "format-version: 1.2",
  "data-version: releases/2026-06-08",
  "ontology: cl",
  "",
  "[Term]",
  "id: CL:0000000",
  "name: cell"
)

test_that("OBO validation rejects non-OBO and non-CL content", {
  html <- tempfile(fileext = ".obo")
  text <- tempfile(fileext = ".obo")
  other_ontology <- tempfile(fileext = ".obo")
  on.exit(unlink(c(html, text, other_ontology)), add = TRUE)

  writeLines(c("<!DOCTYPE html>", "<html><body>Not found</body></html>"), html)
  writeLines("request failed", text)
  writeLines(c("format-version: 1.2", "", "[Term]", "id: GO:0008150"),
             other_ontology)

  expect_error(
    CellOnTools:::.validate_cl_obo_file(html),
    "response appears to be HTML"
  )
  expect_error(
    CellOnTools:::.validate_cl_obo_file(text),
    "missing format-version header"
  )
  expect_error(
    CellOnTools:::.validate_cl_obo_file(other_ontology),
    "missing CL term ID"
  )
})

test_that("OBO validation accepts a structurally valid Cell Ontology file", {
  path <- tempfile(fileext = ".obo")
  on.exit(unlink(path), add = TRUE)
  writeLines(valid_cl_obo, path)

  expect_true(CellOnTools:::.validate_cl_obo_file(path))
})

test_that("pinned OBO validation enforces release and checksum", {
  path <- tempfile(fileext = ".obo")
  on.exit(unlink(path), add = TRUE)
  writeLines(valid_cl_obo, path)
  md5 <- unname(tools::md5sum(path))

  expect_true(CellOnTools:::.validate_cl_obo_file(
    path,
    expected_release = "2026-06-08",
    expected_md5 = md5
  ))
  expect_error(
    CellOnTools:::.validate_cl_obo_file(
      path,
      expected_release = "2026-03-26"
    ),
    "release mismatch"
  )
  expect_error(
    CellOnTools:::.validate_cl_obo_file(
      path,
      expected_release = "2026-06-08",
      expected_md5 = paste(rep("0", 32L), collapse = "")
    ),
    "checksum mismatch"
  )
})

test_that("CLdownload never replaces the destination with invalid content", {
  dest <- tempfile(fileext = ".obo")
  on.exit(unlink(dest), add = TRUE)
  writeLines("existing valid file", dest)

  testthat::local_mocked_bindings(
    .download_obo_file = function(args) {
      writeLines(c("<!DOCTYPE html>", "<html>Not found</html>"), args$destfile)
      0L
    },
    .package = "CellOnTools"
  )

  expect_error(
    suppressMessages(CLdownload(
      dest_file = dest,
      url = "https://example.org/cl.obo",
      overwrite = TRUE
    )),
    "not a valid Cell Ontology OBO document"
  )
  expect_equal(readLines(dest, warn = FALSE), "existing valid file")
})

test_that("CLdownload promotes content only after successful validation", {
  dest <- tempfile(fileext = ".obo")
  unlink(dest)
  on.exit(unlink(dest), add = TRUE)

  testthat::local_mocked_bindings(
    .download_obo_file = function(args) {
      writeLines(valid_cl_obo, args$destfile)
      0L
    },
    .package = "CellOnTools"
  )

  result <- suppressMessages(CLdownload(
    dest_file = dest,
    url = "https://example.org/cl.obo"
  ))

  expect_true(file.exists(dest))
  expect_equal(normalizePath(dest), result)
  expect_equal(readLines(dest, warn = FALSE), valid_cl_obo)
})

test_that("CLdownload retries the fallback URL after invalid primary content", {
  dest <- tempfile(fileext = ".obo")
  unlink(dest)
  on.exit(unlink(dest), add = TRUE)
  attempted_urls <- character()
  reference <- tempfile(fileext = ".obo")
  writeLines(valid_cl_obo, reference)
  on.exit(unlink(reference), add = TRUE)
  fixture_md5 <- unname(tools::md5sum(reference))

  testthat::local_mocked_bindings(
    .cl_release_md5 = function(release) fixture_md5,
    .download_obo_file = function(args) {
      attempted_urls <<- c(attempted_urls, args$url)
      if (grepl("raw.githubusercontent.com", args$url, fixed = TRUE)) {
        writeLines(valid_cl_obo, args$destfile)
      } else {
        writeLines(c("<!DOCTYPE html>", "<html>Proxy error</html>"),
                   args$destfile)
      }
      0L
    },
    .package = "CellOnTools"
  )

  result <- suppressMessages(CLdownload(dest_file = dest))

  expect_equal(result, normalizePath(dest))
  expect_true(any(grepl("raw.githubusercontent.com", attempted_urls,
                        fixed = TRUE)))
  expect_false(any(grepl("/latest/|/master/", attempted_urls)))
  expect_true(all(grepl("2026-06-08", attempted_urls, fixed = TRUE)))
  expect_equal(readLines(dest, warn = FALSE), valid_cl_obo)
})

test_that("default download rejects a wrong release without replacing a file", {
  dest <- tempfile(fileext = ".obo")
  on.exit(unlink(dest), add = TRUE)
  writeLines("existing file", dest)

  wrong_release <- sub(
    "2026-06-08", "2026-03-26", valid_cl_obo,
    fixed = TRUE
  )
  testthat::local_mocked_bindings(
    .cl_release_md5 = function(release) NA_character_,
    .download_obo_file = function(args) {
      writeLines(wrong_release, args$destfile)
      0L
    },
    .package = "CellOnTools"
  )

  expect_error(
    suppressMessages(CLdownload(dest_file = dest, overwrite = TRUE)),
    "release mismatch"
  )
  expect_equal(readLines(dest, warn = FALSE), "existing file")
})

test_that("CLdownload rejects missing and blank scalar arguments early", {
  expect_error(CLdownload(dest_file = NA_character_), "dest_file")
  expect_error(CLdownload(dest_file = "   "), "dest_file")
  expect_error(CLdownload(url = NA_character_), "url")
  expect_error(CLdownload(url = "   "), "url")
})

test_that("validated downloads are committed with same-directory renames", {
  directory <- tempfile("cellontools-commit-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)

  dest <- file.path(directory, "cl.obo")
  incoming <- file.path(directory, "incoming.download")
  writeLines("old", dest)
  writeLines("new", incoming)

  expect_no_error(
    CellOnTools:::.commit_cl_download(incoming, dest, overwrite = TRUE)
  )
  expect_equal(readLines(dest, warn = FALSE), "new")
  expect_false(file.exists(incoming))
  expect_length(list.files(directory, pattern = "backup", all.files = TRUE), 0L)
})

test_that("download destination locks are acquired and released", {
  directory <- tempfile("cellontools-lock-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)
  dest <- file.path(directory, "cl.obo")

  lock <- CellOnTools:::.acquire_cl_file_lock(dest, timeout = 1)
  expect_true(dir.exists(lock))
  expect_true(file.exists(file.path(lock, "owner")))

  CellOnTools:::.release_cl_file_lock(lock)
  expect_false(dir.exists(lock))
})

test_that("download retries respect a bounded total time budget", {
  dest <- tempfile(fileext = ".obo")
  on.exit(unlink(dest), add = TRUE)
  old_options <- options(
    CellOnTools.download_timeout = 1,
    CellOnTools.download_total_timeout = 1,
    timeout = 17,
    download.file.method = "auto"
  )
  on.exit(options(old_options), add = TRUE)

  testthat::local_mocked_bindings(
    .download_obo_file = function(args) {
      Sys.sleep(1.05)
      stop("mock network timeout")
    },
    .package = "CellOnTools"
  )

  expect_error(
    suppressMessages(CLdownload(
      dest_file = dest,
      url = "https://example.invalid/cl.obo"
    )),
    "total download time budget of 1 seconds exceeded"
  )
  expect_identical(getOption("timeout"), 17)
})

test_that("auto download method is not retried as explicit libcurl", {
  dest <- tempfile(fileext = ".obo")
  on.exit(unlink(dest), add = TRUE)
  old_options <- options(
    CellOnTools.download_timeout = 5,
    CellOnTools.download_total_timeout = 30,
    download.file.method = "auto"
  )
  on.exit(options(old_options), add = TRUE)
  attempted_methods <- character()

  testthat::local_mocked_bindings(
    .download_obo_file = function(args) {
      attempted_methods <<- c(attempted_methods, args$method)
      stop("mock failure")
    },
    .package = "CellOnTools"
  )

  expect_error(
    suppressMessages(CLdownload(
      dest_file = dest,
      url = "https://example.invalid/cl.obo"
    )),
    "Download failed"
  )
  expect_true("auto" %in% attempted_methods)
  expect_false("libcurl" %in% attempted_methods)
})

test_that("download timeout options reject invalid values", {
  dest <- tempfile(fileext = ".obo")
  on.exit(unlink(dest), add = TRUE)
  old_options <- options(CellOnTools.download_timeout = 0)
  on.exit(options(old_options), add = TRUE)

  expect_error(
    suppressMessages(CLdownload(
      dest_file = dest,
      url = "https://example.invalid/cl.obo"
    )),
    "CellOnTools.download_timeout"
  )
})
