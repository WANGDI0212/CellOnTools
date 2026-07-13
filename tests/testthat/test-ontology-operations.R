test_that("ontology ancestor, descendant, and depth helpers work on a simple DAG", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_data()

  expect_equal(CLdepth("CL:0000003", cl), stats::setNames(3L, "CL:0000003"))
  expect_warning(depth <- CLdepth(c("CL:0000003", "CL:9999999"), cl), "Unknown CL ID")
  expect_equal(unname(depth["CL:0000003"]), 3L)
  expect_true(is.na(depth["CL:9999999"]))

  ancestors <- CLancestors("CL:0000003", cl)[["CL:0000003"]]
  expect_equal(ancestors, c("CL:0000002", "CL:0000001", "CL:0000000"))

  descendants <- CLdescendants("CL:0000001", cl)[["CL:0000001"]]
  expect_equal(descendants, c("CL:0000002", "CL:0000003"))

  expect_equal(
    CLcommonAncestor(c("CL:0000002", "CL:0000003"), cl, most_specific = TRUE),
    "CL:0000002"
  )
})

test_that("hierarchy extraction returns graph-ready nodes and edges", {
  testthat::skip_if_not_installed("ontologyIndex")
  cl <- test_cl_data()

  hierarchy <- CLhierarchy("CL:0000003", cl, max_hops = 2)
  expect_setequal(hierarchy$nodes$id, c("CL:0000001", "CL:0000002", "CL:0000003"))
  expect_setequal(hierarchy$edges$from, c("CL:0000002", "CL:0000003"))
  expect_true(hierarchy$nodes$is_query[hierarchy$nodes$id == "CL:0000003"])
})

test_that("CLhierarchyPlot exposes and forwards hop distance", {
  expect_true("max_hops" %in% names(formals(CLhierarchyPlot)))
  expect_false("max_ancestor_count" %in% names(formals(CLhierarchyPlot)))

  testthat::skip_if_not_installed("ggraph")
  testthat::skip_if_not_installed("igraph")
  testthat::skip_if_not_installed("ggplot2")
  cl <- test_cl_dag()
  plot <- CLhierarchyPlot("CL:0000700", cl, max_hops = 1)
  expect_true(inherits(plot, "ggplot"))
})
