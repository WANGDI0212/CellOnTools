# Argument-validation paths that fail fast, before any network or optional
# dependency is touched.  These run without ontologyIndex / AnnotationHub / rols.

test_that(".validate_cldata rejects non-ontology objects", {
  expect_error(CLid2label("CL:0000084", clData = NULL), "must be provided")
  expect_error(CLid2label("CL:0000084", clData = list(a = 1)),
               "ontology_index object")
})

test_that("CLload validates its arguments before contacting any source", {
  expect_error(CLload(prefer_local = TRUE), "requires `local_obo`")
  expect_error(CLload(yearAdded = 2023), "non-empty character string")
  expect_error(CLload(local_obo = "definitely-missing-file.obo"),
               "does not exist")

  # Existing file with the wrong extension is rejected.
  tmp <- tempfile(fileext = ".txt")
  writeLines("not an obo", tmp)
  on.exit(unlink(tmp), add = TRUE)
  expect_error(CLload(local_obo = tmp), "must be an .obo file")
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
