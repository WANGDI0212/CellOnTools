test_that("bundled marker data load with the expected schema", {
  required <- c("species", "CL_ID", "CL_label", "marker_symbol", "marker_entrezid")

  human <- CLmarkers("human", check_unique = FALSE)
  mouse <- CLmarkers("mouse", check_unique = FALSE)

  expect_s3_class(human, "data.frame")
  expect_s3_class(mouse, "data.frame")
  expect_named(human, required)
  expect_named(mouse, required)
  expect_gt(nrow(human), 0L)
  expect_gt(nrow(mouse), 0L)
  expect_true(all(human$species == "Human"))
  expect_true(all(mouse$species == "Mouse"))
})
