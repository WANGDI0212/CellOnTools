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

  # `groups` contains only the groups accepted by the greedy assignment,
  # using the terms actually assigned to each ancestor.
  expect_named(res$groups, c("CL:0000300", "CL:0000100"))
  expect_setequal(
    res$groups[["CL:0000300"]],
    c("CL:0000300", "CL:0000700")
  )
  expect_setequal(
    res$groups[["CL:0000100"]],
    c("CL:0000400", "CL:0000600")
  )
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
  expect_length(res$groups, 0L)
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

  expect_error(
    CLrollup(c("CL:0000300", "not_an_id"), cl, verbose = FALSE),
    "Invalid CL ID format"
  )

  for (value in list(2.5, Inf, NaN, .Machine$integer.max + 1)) {
    expect_error(
      CLrollup(c("CL:0000300", "CL:0000400"), cl,
               min_group_size = value, verbose = FALSE),
      "finite positive integer"
    )
  }

  for (value in list(-1, 2.5, Inf, NaN, .Machine$integer.max + 1)) {
    expect_error(
      CLrollup(c("CL:0000300", "CL:0000400"), cl,
               max_candidate_ancestor_count = value, verbose = FALSE),
      "finite non-negative integer"
    )
  }

  for (value in list(NA, 1, c(TRUE, FALSE))) {
    expect_error(
      CLrollup(c("CL:0000300", "CL:0000400"), cl,
               return_mapping = value, verbose = FALSE),
      "`return_mapping` must be TRUE or FALSE"
    )
    expect_error(
      CLrollup(c("CL:0000300", "CL:0000400"), cl, verbose = value),
      "`verbose` must be TRUE or FALSE"
    )
  }
})

test_that("candidate ancestor cap is applied before redundant ancestors are removed", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_dag()
  ids <- c("CL:0000300", "CL:0000700") # T cell + its NKT descendant

  # T cell (ancestor_count 3) covers both inputs and has the same coverage as
  # lymphocyte (ancestor_count 2).  With a cap of 2, T cell is ineligible and
  # lymphocyte must remain available as the most specific eligible candidate.
  res <- CLrollup(
    ids, cl, min_group_size = 2,
    max_candidate_ancestor_count = 2, verbose = FALSE
  )

  expect_equal(res$mapping$rolled_id, rep("CL:0000200", 2L))
  expect_equal(res$rolled_terms, "CL:0000200")
  expect_named(res$groups, "CL:0000200")
  expect_setequal(res$groups[["CL:0000200"]], ids)

  # Zero is a meaningful cap because the ontology root has ancestor_count 0.
  root_only <- CLrollup(
    c("CL:0000300", "CL:0000400"), cl,
    min_group_size = 2, max_candidate_ancestor_count = 0, verbose = FALSE
  )
  expect_equal(root_only$mapping$rolled_id, rep("CL:0000000", 2L))
  expect_equal(root_only$rolled_terms, "CL:0000000")
  expect_named(root_only$groups, "CL:0000000")
  expect_setequal(
    root_only$groups[["CL:0000000"]],
    c("CL:0000300", "CL:0000400")
  )
})

test_that("CLrollup never selects imported ontology ancestors", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_cross_ontology()
  ids <- c("CL:0000084", "CL:0000798")

  root_only <- CLrollup(
    ids,
    cl,
    min_group_size = 2,
    max_candidate_ancestor_count = 0,
    verbose = FALSE
  )
  expect_equal(root_only$mapping$rolled_id, rep("CL:0000000", 2L))
  expect_true(all(grepl("^CL:", root_only$mapping$rolled_id)))
  expect_true(all(grepl("^CL:", names(root_only$groups))))

  # These CL terms share only imported BFO ancestors, so there is no eligible
  # shared CL roll-up candidate.
  external_only <- CLrollup(
    c("CL:1000000", "CL:1000001"),
    cl,
    min_group_size = 2,
    verbose = FALSE
  )
  expect_true(all(!external_only$mapping$was_rolled))
  expect_length(external_only$groups, 0L)
})

test_that("groups agrees with the final greedy assignment on overlapping DAG candidates", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_dag()
  ids <- c("CL:0000300", "CL:0000350", "CL:0000700", "CL:0000400")

  res <- CLrollup(ids, cl, min_group_size = 2, verbose = FALSE)
  final_groups <- split(res$mapping$original_id, res$mapping$rolled_id)
  final_groups <- final_groups[lengths(final_groups) >= 2L]

  expect_setequal(names(res$groups), names(final_groups))
  for (ancestor in names(final_groups)) {
    expect_equal(
      sort(res$groups[[ancestor]]),
      sort(final_groups[[ancestor]])
    )
  }

  # NK cell is a valid overlapping candidate for NKT cell, but it loses the
  # deterministic greedy tie-break and therefore must not be reported.
  expect_false("CL:0000350" %in% names(res$groups))
})

test_that("set semantics and min_group_size one are explicit", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_dag()

  cleaned <- CLrollup(
    c(NA, "", "CL:0000300", "CL:0000300", "CL:0000400"),
    cl, min_group_size = 2, verbose = FALSE
  )
  expect_equal(
    cleaned$mapping$original_id,
    c("CL:0000300", "CL:0000400")
  )

  singleton <- CLrollup(
    c("CL:0000300", "CL:0000400"),
    cl, min_group_size = 1, verbose = FALSE
  )
  expect_true(all(!singleton$mapping$was_rolled))
  expect_setequal(names(singleton$groups), c("CL:0000300", "CL:0000400"))
  expect_equal(singleton$groups[["CL:0000300"]], "CL:0000300")
  expect_equal(singleton$groups[["CL:0000400"]], "CL:0000400")
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
  expect_gte(sug$best$min_group_size, 1L)
  expect_named(
    sug$best,
    c(
      "min_group_size", "max_candidate_ancestor_count", "n_groups",
      "n_rolled", "compression_ratio", "deviation", "error"
    )
  )
  # The recommended configuration should be the one closest to the target.
  devs <- vapply(sug$all_candidates, function(r)
    if (is.na(r$deviation)) Inf else r$deviation, numeric(1))
  expect_equal(sug$best$deviation, min(devs))
})

test_that("CLsuggestResolution includes identity resolution", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_dag()
  ids <- c("CL:0000300", "CL:0000700")

  suggestion <- CLsuggestResolution(
    ids, cl, target_groups = 2, verbose = FALSE
  )

  expect_equal(suggestion$best$min_group_size, 1L)
  expect_equal(suggestion$best$n_groups, 2L)
  expect_equal(suggestion$best$deviation, 0L)
  expect_setequal(
    vapply(suggestion$all_candidates, `[[`, integer(1L), "min_group_size"),
    seq_along(ids)
  )
})

test_that("CLsuggestResolution does not truncate the upper sweep bound", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_dag()
  ids <- c(
    "CL:0000000", "CL:0000100", "CL:0000200",
    "CL:0000300", "CL:0000400"
  )

  # On this overlapping hierarchy, group counts for min_group_size 1:5 are
  # 5, 2, 3, 2, 1. The former target-derived upper bound was 2 and therefore
  # missed the exact three-group solution at min_group_size = 3.
  suggestion <- CLsuggestResolution(
    ids, cl, target_groups = 3, verbose = FALSE
  )

  expect_equal(suggestion$best$min_group_size, 3L)
  expect_equal(suggestion$best$n_groups, 3L)
  expect_equal(suggestion$best$deviation, 0L)
  expect_setequal(
    vapply(suggestion$all_candidates, `[[`, integer(1L), "min_group_size"),
    seq_along(ids)
  )
})

test_that("CLsuggestResolution validates IDs and candidate caps", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_dag()

  expect_error(
    CLsuggestResolution(
      c("CL:0000300", "CL:9999999"), cl, verbose = FALSE
    ),
    "Unknown CL ID"
  )
  for (caps in list(c(1, Inf), -1, 1.5, NA_real_)) {
    expect_error(
      CLsuggestResolution(
        c("CL:0000300", "CL:0000400"),
        cl,
        max_candidate_ancestor_count_values = caps,
        verbose = FALSE
      ),
      "finite non-negative integers"
    )
  }

  accepted <- CLsuggestResolution(
    c("CL:0000300", "CL:0000400"),
    cl,
    target_groups = 1,
    max_candidate_ancestor_count_values = c(0L, 0L, 2L),
    verbose = FALSE
  )
  expect_true(any(vapply(
    accepted$all_candidates,
    function(x) identical(x$max_candidate_ancestor_count, 0L),
    logical(1L)
  )))

  expect_error(
    CLsuggestResolution(
      c("CL:0000300", "CL:0000400"), cl,
      target_groups = 1.5, verbose = FALSE
    ),
    "finite positive integer"
  )
  expect_error(
    CLsuggestResolution(
      c("CL:0000300", "CL:0000400"), cl,
      target_groups = 1, verbose = NA
    ),
    "must be TRUE or FALSE"
  )
})
