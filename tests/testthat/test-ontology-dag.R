# DAG-specific behaviour that a linear chain cannot exercise: multi-parent
# ancestry, multiple "most specific" common ancestors, and depth as
# ancestor-count rather than path length.

test_that("ancestors and depth follow ancestor-count semantics on a DAG", {
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
})

test_that("descendants include indirectly-reachable DAG nodes", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_dag()

  desc <- CLdescendants("CL:0000200", cl)[["CL:0000200"]]
  expect_setequal(
    desc,
    c("CL:0000300", "CL:0000400", "CL:0000350", "CL:0000700")
  )

  # max_descendant_ancestor_count = 1 keeps only direct children.
  direct <- CLdescendants("CL:0000200", cl,
                          max_descendant_ancestor_count = 1)[["CL:0000200"]]
  expect_setequal(direct, c("CL:0000300", "CL:0000400", "CL:0000350"))
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

test_that("hierarchy filtering uses ancestor-count distance from deepest query", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_dag()

  # Deepest query (monocyte) has ancestor_count 3; keeping ancestors within 1
  # step retains only ancestors with ancestor_count >= 2.
  hier <- CLhierarchy("CL:0000600", cl, max_ancestor_count = 1)
  expect_setequal(hier$nodes$id, c("CL:0000600", "CL:0000500"))
  expect_equal(nrow(hier$edges), 1L)
  expect_equal(hier$edges$from, "CL:0000600")
  expect_equal(hier$edges$to, "CL:0000500")
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
