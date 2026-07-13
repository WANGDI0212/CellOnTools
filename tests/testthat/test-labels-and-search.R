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

test_that("label conversion and search exclude imported ontology terms", {
  cl <- test_cl_cross_ontology()

  expect_warning(
    imported <- CLlabel2id("continuant", cl),
    "Unknown cell type"
  )
  expect_true(is.na(imported))

  expect_warning(
    hits <- CLsearchLabel("continuant", cl, exact_match = TRUE),
    "No matches found"
  )
  expect_equal(nrow(hits), 0L)

  # An imported term with the same label must not displace the CL term.
  cl$name["BFO:0000002"] <- "cell"
  expect_equal(CLlabel2id("cell", cl), "CL:0000000")
  hit <- CLsearchLabel("cell", cl, exact_match = TRUE, max_results = 1)
  expect_equal(hit$id, "CL:0000000")
  expect_true(all(grepl("^CL:", hit$id)))
})
