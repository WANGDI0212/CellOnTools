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
