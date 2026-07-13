# Argument-validation paths that fail fast, before any network or optional
# dependency is touched.  These run without ontologyIndex / AnnotationHub / rols.

test_that(".validate_cldata rejects non-ontology objects", {
  expect_error(CLid2label("CL:0000084", clData = NULL), "must be provided")
  expect_error(CLid2label("CL:0000084", clData = list(a = 1)),
               "ontology_index object")

  incomplete <- structure(
    list(id = "CL:0000000", name = c("CL:0000000" = "cell")),
    class = "ontology_index"
  )
  expect_error(
    CLid2label("CL:0000000", clData = incomplete),
    "missing required field"
  )
})

test_that("CLload validates its arguments before contacting any source", {
  expect_error(CLload(prefer_local = TRUE), "requires `local_obo`")
  expect_error(CLload(yearAdded = 2023), "non-empty character string")
  expect_error(CLload(release = "2026"), "YYYY-MM-DD")
  expect_error(
    CLload(yearAdded = "2023", release = "2026-06-08"),
    "only one"
  )
  expect_error(CLload(local_obo = "definitely-missing-file.obo"),
               "does not exist")

  # Existing file with the wrong extension is rejected.
  tmp <- tempfile(fileext = ".txt")
  writeLines("not an obo", tmp)
  on.exit(unlink(tmp), add = TRUE)
  expect_error(CLload(local_obo = tmp), "must be an .obo file")
})

test_that("CLload uses the pinned release cache when yearAdded is omitted", {
  testthat::skip_if_not_installed("ontologyIndex")

  cache_dir <- tempfile("cellontools-cache-")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  old_options <- options(CellOnTools.cache_dir = cache_dir)
  on.exit(options(old_options), add = TRUE)

  cache_file <- CellOnTools:::.cl_cache_file("2026-06-08")
  cache_obo <- c(
    "format-version: 1.2",
    "data-version: releases/2026-06-08",
    "ontology: cl",
    "",
    "[Term]",
    "id: CL:0000000",
    "name: cell"
  )
  writeLines(cache_obo, cache_file)
  fixture_md5 <- unname(tools::md5sum(cache_file))

  testthat::local_mocked_bindings(
    .cl_release_md5 = function(release) fixture_md5,
    .package = "CellOnTools"
  )

  cl <- CLload(verbose = FALSE)
  expect_s3_class(cl, "ontology_index")
  expect_equal(attr(cl, "ontology_release"), "2026-06-08")
  expect_equal(attr(cl, "ontology_source"), normalizePath(
    cache_file, winslash = "/", mustWork = TRUE
  ))
})

test_that("CLdownload validates URL and overwrite before downloading", {
  expect_error(CLdownload(url = "ftp://example.org/cl.obo"),
               "http://")

  tmp <- tempfile(fileext = ".obo")
  writeLines("format-version: 1.2", tmp)
  on.exit(unlink(tmp), add = TRUE)
  expect_error(CLdownload(dest_file = tmp, overwrite = FALSE),
               "already exists")
})

test_that("CLmap rejects incompatible returnType / max_results", {
  expect_error(CLmap("T cell", returnType = "id", max_results = 5),
               "returnType must be 'all'")
  expect_error(CLmap(character(0)), "non-empty character vector")
})

test_that("CLsearchLabel validates patterns and max_results", {
  cl <- test_cl_data()
  expect_error(CLsearchLabel(character(0), cl), "non-empty character vector")
  expect_error(CLsearchLabel("cell", cl, max_results = 0),
               "positive integer")
})
