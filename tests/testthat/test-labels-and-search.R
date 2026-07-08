test_that("CL ID and label conversion preserve order, names, and missing values", {
  cl <- test_cl_data()

  ids <- c(sample_a = "CL:0000003", sample_b = "CL:9999999", sample_c = "")
  expect_warning(labels <- CLid2label(ids, cl), "Unknown CL ID")
  expect_equal(unname(labels["sample_a"]), "T cell")
  expect_true(is.na(labels["sample_b"]))
  expect_true(is.na(labels["sample_c"]))

  expect_equal(
    CLlabel2id(c("T cell", "t cell"), cl, ignore_case = TRUE),
    c("CL:0000003", "CL:0000003")
  )
  expect_error(CLlabel2id("unknown cell", cl, strict = TRUE), "Unknown cell type")
})

test_that("CL label search supports literal and exact matching", {
  cl <- test_cl_data()

  partial <- CLsearchLabel("cell", cl)
  expect_named(partial, c("pattern", "id", "label", "search_mode"))
  expect_setequal(partial$id, c("CL:0000000", "CL:0000001", "CL:0000003"))
  expect_equal(unique(partial$search_mode), "partial")

  exact <- CLsearchLabel("T cell", cl, exact_match = TRUE)
  expect_equal(exact$id, "CL:0000003")
  expect_equal(exact$search_mode, "exact")

  expect_warning(empty <- CLsearchLabel("does not exist", cl), "No matches found")
  expect_named(empty, c("pattern", "id", "label", "search_mode"))
  expect_equal(nrow(empty), 0L)
})
