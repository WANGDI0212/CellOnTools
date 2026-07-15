test_that("release metadata is fixed and normalised consistently", {
  expect_equal(CellOnTools:::.CL_RELEASE, "2026-06-08")
  expect_equal(
    CellOnTools:::.normalise_cl_release(
      "data-version: releases/2026-06-08"
    ),
    "2026-06-08"
  )
  expect_equal(
    CellOnTools:::.normalise_cl_release("v2026-06-08"),
    "2026-06-08"
  )

  urls <- CellOnTools:::.cl_release_urls()
  expect_true(all(grepl("2026-06-08", urls, fixed = TRUE)))
  expect_false(any(grepl("/latest/|/master/", urls)))
  expect_equal(
    CellOnTools:::.cl_release_md5("2026-06-08"),
    "79fcc8bc4dfa70e5de6d3912bcba1f95"
  )
})

test_that("AnnotationHub year selection requires an exact four-digit year", {
  expect_error(CLload(yearAdded = "."), "four-digit year")
  expect_error(CLload(yearAdded = "["), "four-digit year")
  expect_error(CLload(yearAdded = "202"), "four-digit year")
  expect_error(CLload(yearAdded = "20230"), "four-digit year")
})

test_that("OLS release guard accepts only the pinned release", {
  testthat::local_mocked_bindings(
    .ols_cl_version = function() "2026-06-08",
    .package = "CellOnTools"
  )
  expect_equal(
    CellOnTools:::.assert_ols_cl_release(),
    "2026-06-08"
  )
})

test_that("OLS release guard rejects release drift", {
  testthat::local_mocked_bindings(
    .ols_cl_version = function() "2026-09-01",
    .package = "CellOnTools"
  )
  expect_error(
    CellOnTools:::.assert_ols_cl_release(),
    "pinned to 2026-06-08"
  )
})

test_that("OLS release guard reports version lookup failures", {
  testthat::local_mocked_bindings(
    .ols_cl_version = function() stop("service unavailable"),
    .package = "CellOnTools"
  )
  expect_error(
    CellOnTools:::.assert_ols_cl_release(),
    "Could not verify.*service unavailable"
  )
})

test_that("mapping functions stop before search when OLS release drifts", {
  testthat::skip_if_not_installed("rols")
  testthat::local_mocked_bindings(
    .ols_cl_version = function() "2026-09-01",
    .package = "CellOnTools"
  )

  expect_error(
    CLmap("T cell", verbose = FALSE),
    "pinned to 2026-06-08"
  )
  expect_error(
    CLmapInteractive("T cell", auto_accept = TRUE),
    "pinned to 2026-06-08"
  )
})

test_that("release cache is rechecked after acquiring the writer lock", {
  cache_file <- tempfile(fileext = ".obo")
  on.exit(unlink(cache_file), add = TRUE)
  download_calls <- 0L

  testthat::local_mocked_bindings(
    .cl_cache_file = function(release) cache_file,
    .validate_cl_obo_file = function(path, expected_release = NULL,
                                     expected_md5 = NULL) {
      if (!file.exists(path)) stop("missing cache")
      invisible(TRUE)
    },
    .acquire_cl_file_lock = function(dest_file, ...) {
      writeLines("populated by another process", cache_file)
      paste0(cache_file, ".lock")
    },
    .release_cl_file_lock = function(lock_dir) invisible(NULL),
    .download_cl_obo_locked = function(...) {
      download_calls <<- download_calls + 1L
    },
    .load_cl_obo_file = function(...) "loaded",
    .package = "CellOnTools"
  )

  result <- CellOnTools:::.load_cl_release(verbose = FALSE)
  expect_identical(result, "loaded")
  expect_identical(download_calls, 0L)
})
