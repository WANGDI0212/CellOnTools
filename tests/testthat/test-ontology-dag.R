# DAG-specific behaviour that a linear chain cannot exercise: multi-parent
# graph hops, multiple "most specific" common ancestors, and ancestor count as
# a distinct specificity measure.

test_that("ancestors use graph hops while depth uses CL ancestor count", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_dag()

  # NKT cell has two parents (T cell + NK cell); all five proper ancestors
  # must be returned exactly once.
  anc <- CLancestors("CL:0000700", cl)[["CL:0000700"]]
  expect_setequal(
    anc,
    c("CL:0000300", "CL:0000350", "CL:0000200", "CL:0000100", "CL:0000000")
  )

  # Depth = number of unique proper ancestors (NOT shortest/longest path).
  depths <- CLdepth(c("CL:0000700", "CL:0000300", "CL:0000000"), cl)
  expect_equal(unname(depths["CL:0000700"]), 5L)
  expect_equal(unname(depths["CL:0000300"]), 3L)
  expect_equal(unname(depths["CL:0000000"]), 0L)

  # Both direct parents must be returned at one hop even though their
  # ancestor-count differences from NKT are greater than one.
  direct <- CLancestors("CL:0000700", cl, max_hops = 1)[["CL:0000700"]]
  expect_setequal(direct, c("CL:0000300", "CL:0000350"))

  expect_length(
    CLancestors("CL:0000700", cl, max_hops = 0)[["CL:0000700"]],
    0L
  )
  expect_equal(
    CLancestors(
      "CL:0000700", cl, include_self = TRUE, max_hops = 0
    )[["CL:0000700"]],
    "CL:0000700"
  )
  expect_setequal(
    CLancestors("CL:0000700", cl, max_hops = 2)[["CL:0000700"]],
    c("CL:0000300", "CL:0000350", "CL:0000200")
  )
})

test_that("descendants include indirectly-reachable DAG nodes", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_dag()

  desc <- CLdescendants("CL:0000200", cl)[["CL:0000200"]]
  expect_setequal(
    desc,
    c("CL:0000300", "CL:0000400", "CL:0000350", "CL:0000700")
  )

  direct <- CLdescendants(
    "CL:0000200", cl, max_hops = 1
  )[["CL:0000200"]]
  expect_setequal(
    direct,
    c("CL:0000300", "CL:0000400", "CL:0000350")
  )

  # NKT is a direct child of T cell despite a two-count specificity gap.
  expect_equal(
    CLdescendants("CL:0000300", cl, max_hops = 1)[["CL:0000300"]],
    "CL:0000700"
  )
})

test_that("common ancestor respects DAG topology", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_dag()

  # T cell + B cell -> lymphocyte is the most specific shared node.
  expect_equal(
    CLcommonAncestor(c("CL:0000300", "CL:0000400"), cl, most_specific = TRUE),
    "CL:0000200"
  )

  # A term that is an ancestor of the other is itself the MSCA.
  expect_equal(
    CLcommonAncestor(c("CL:0000700", "CL:0000300"), cl, most_specific = TRUE),
    "CL:0000300"
  )
})

test_that("hierarchy includes direct parents for every query", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_dag()

  hier <- CLhierarchy(
    c("CL:0000700", "CL:0000600"), cl, max_hops = 1
  )
  expect_setequal(
    hier$nodes$id,
    c(
      "CL:0000700", "CL:0000600", "CL:0000300",
      "CL:0000350", "CL:0000500"
    )
  )
  expect_setequal(
    paste(hier$edges$from, hier$edges$to),
    c(
      "CL:0000700 CL:0000300",
      "CL:0000700 CL:0000350",
      "CL:0000600 CL:0000500"
    )
  )
})

test_that("hop traversal and ancestor counts stay within the CL namespace", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_cross_ontology()

  expect_equal(
    CLancestors("CL:0000798", cl, max_hops = 1)[["CL:0000798"]],
    "CL:0000084"
  )
  expect_equal(
    CLdescendants("CL:0000084", cl, max_hops = 1)[["CL:0000084"]],
    "CL:0000798"
  )
  expect_equal(
    unname(CLdepth(c("CL:0000000", "CL:0000084", "CL:0000798"), cl)),
    c(0L, 1L, 2L)
  )
  expect_true(all(grepl(
    "^CL:",
    CLancestors("CL:0000798", cl)[["CL:0000798"]]
  )))
  expect_equal(
    CLcommonAncestor(
      c("CL:0000084", "CL:0000798"), cl, most_specific = TRUE
    ),
    "CL:0000084"
  )
  expect_length(
    CLcommonAncestor(c("CL:1000000", "CL:1000001"), cl),
    0L
  )
  expect_length(
    CLancestors("CL:1000000", cl)[["CL:1000000"]],
    0L
  )
})

test_that("imported ontology IDs are rejected from CL graph queries", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_cross_ontology()

  expect_warning(
    depth <- CLdepth("BFO:0000002", cl),
    "Invalid CL ID format"
  )
  expect_true(is.na(unname(depth)))

  expect_warning(
    ancestors <- CLancestors("BFO:0000002", cl, include_self = TRUE),
    "Invalid CL ID format"
  )
  expect_length(ancestors[[1L]], 0L)

  expect_warning(
    descendants <- CLdescendants("CARO:0000006", cl),
    "Invalid CL ID format"
  )
  expect_length(descendants[[1L]], 0L)
})

test_that("hop limits require finite non-negative integers", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_dag()

  for (value in list(-1, 1.5, Inf, NA_real_)) {
    expect_error(
      CLancestors("CL:0000700", cl, max_hops = value),
      "finite non-negative integer"
    )
    expect_error(
      CLdescendants("CL:0000300", cl, max_hops = value),
      "finite non-negative integer"
    )
  }
})

test_that("zero-hop hierarchy retains only query nodes", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_dag()

  hier <- CLhierarchy("CL:0000700", cl, max_hops = 0)
  expect_equal(hier$nodes$id, "CL:0000700")
  expect_equal(nrow(hier$edges), 0L)
  expect_error(
    CLhierarchy("CL:0000700", cl, include_ancestors = NA),
    "must be TRUE or FALSE"
  )
})

test_that("malformed IDs warn exactly once (no duplicate unknown warning)", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_dag()

  # Regression test: a bad-format ID must trigger only the format warning,
  # not an additional "Unknown CL ID" warning for the same value.
  warns <- capture_warnings(CLancestors(c("CL:0000300", "not_an_id"), cl))
  expect_length(warns, 1L)
  expect_match(warns, "Invalid CL ID format")
  expect_false(any(grepl("Unknown", warns)))

  # A well-formed but absent ID still produces the unknown warning (once).
  warns2 <- capture_warnings(CLdepth(c("CL:0000300", "CL:9999999"), cl))
  expect_length(warns2, 1L)
  expect_match(warns2, "Unknown CL ID")
})
