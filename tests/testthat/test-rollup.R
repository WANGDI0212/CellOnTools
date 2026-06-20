test_that("CLrollup aggregates terms to their most specific shared ancestor", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_dag()

  # T cell, B cell, monocyte, NKT cell with min_group_size = 2.
  res <- CLrollup(
    c("CL:0000300", "CL:0000400", "CL:0000600", "CL:0000700"),
    cl, min_group_size = 2, verbose = FALSE
  )

  expect_named(res, c("mapping", "groups", "rolled_terms"))
  map <- res$mapping
  rolled <- stats::setNames(map$rolled_id, map$original_id)

  # NKT rolls up to its parent T cell (the most specific 2+ member ancestor);
  # T cell stays put; B cell and monocyte share only "immune cell".
  expect_equal(unname(rolled["CL:0000700"]), "CL:0000300")
  expect_equal(unname(rolled["CL:0000300"]), "CL:0000300")
  expect_equal(unname(rolled["CL:0000400"]), "CL:0000100")
  expect_equal(unname(rolled["CL:0000600"]), "CL:0000100")

  expect_equal(map$was_rolled, map$original_id != map$rolled_id)
})

test_that("CLrollup leaves terms untouched when no group meets min_group_size", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_dag()

  expect_warning(
    res <- CLrollup(c("CL:0000300", "CL:0000400"), cl,
                    min_group_size = 5, verbose = FALSE),
    "larger than the number of input IDs"
  )
  # No roll-up occurs: every term maps to itself.
  expect_true(all(!res$mapping$was_rolled))
})

test_that("CLrollup return_mapping = FALSE returns a named vector", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_dag()

  out <- CLrollup(
    c("CL:0000300", "CL:0000400", "CL:0000600", "CL:0000700"),
    cl, min_group_size = 2, return_mapping = FALSE, verbose = FALSE
  )
  expect_type(out, "character")
  expect_named(out)
  expect_equal(unname(out["CL:0000700"]), "CL:0000300")
})

test_that("CLrollup rejects unknown IDs and invalid parameters", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_dag()

  expect_error(
    CLrollup(c("CL:0000300", "CL:9999999"), cl, verbose = FALSE),
    "Unknown CL ID"
  )
  expect_error(
    CLrollup(c("CL:0000300", "CL:0000400"), cl, min_group_size = 0,
             verbose = FALSE),
    "positive integer"
  )
})

test_that("CLsuggestResolution finds a configuration near the target", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_dag()

  sug <- CLsuggestResolution(
    c("CL:0000300", "CL:0000400", "CL:0000600", "CL:0000700", "CL:0000350"),
    cl, target_groups = 2, verbose = FALSE
  )
  expect_named(sug, c("best", "all_candidates"))
  expect_true(is.numeric(sug$best$n_groups))
  expect_gte(sug$best$min_group_size, 2L)
  # The recommended configuration should be the one closest to the target.
  devs <- vapply(sug$all_candidates, function(r)
    if (is.na(r$deviation)) Inf else r$deviation, numeric(1))
  expect_equal(sug$best$deviation, min(devs))
})
