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

  expect_equal(nrow(human), 140337L)
  expect_equal(nrow(mouse), 49289L)
  expect_equal(length(unique(human$CL_ID)), 723L)
  expect_equal(length(unique(mouse$CL_ID)), 491L)
})

test_that("bundled markers use active terms from the pinned CL release", {
  human <- CLmarkers("human", check_unique = FALSE)
  mouse <- CLmarkers("mouse", check_unique = FALSE)
  obsolete <- names(CellOnTools:::.CL_MARKER_REPLACEMENTS)
  replacements <- unname(CellOnTools:::.CL_MARKER_REPLACEMENTS)

  expect_false(any(human$CL_ID %in% obsolete))
  expect_false(any(mouse$CL_ID %in% obsolete))
  expect_true(all(replacements %in% human$CL_ID))
  expect_true("CL:0000099" %in% mouse$CL_ID)
  expect_true(all(grepl("^CL:\\d+$", human$CL_ID)))
  expect_true(all(grepl("^CL:\\d+$", mouse$CL_ID)))
  expect_false(any(startsWith(human$CL_label, "obsolete ")))
  expect_false(any(startsWith(mouse$CL_label, "obsolete ")))

  human_terms <- unique(human[c("CL_ID", "CL_label")])
  mouse_terms <- unique(mouse[c("CL_ID", "CL_label")])
  expect_equal(anyDuplicated(human_terms$CL_ID), 0L)
  expect_equal(anyDuplicated(mouse_terms$CL_ID), 0L)
})

test_that("bundled markers carry exact ontology and source provenance", {
  human <- CLmarkers("human", check_unique = FALSE)
  mouse <- CLmarkers("mouse", check_unique = FALSE)

  for (x in list(human, mouse)) {
    expect_equal(attr(x, "ontology_release"), "2026-06-08")
    expect_equal(
      attr(x, "ontology_url"),
      "https://purl.obolibrary.org/obo/cl/releases/2026-06-08/cl.obo"
    )
    expect_equal(
      attr(x, "ontology_md5"),
      "79fcc8bc4dfa70e5de6d3912bcba1f95"
    )
    expect_equal(
      attr(x, "marker_source_file"),
      "TheCellMarkerAccordion_database_v1.0.0.xlsx"
    )
    expect_equal(
      attr(x, "marker_source_repository"),
      "https://github.com/TebaldiLab/shiny_cellmarkeraccordion"
    )
    expect_equal(
      attr(x, "marker_source_commit"),
      "a2cc870a40df2cdd8f2c9671605b19e3f29229d7"
    )
    expect_equal(
      attr(x, "marker_source_sha256"),
      "53ec885a4e3844c8493d3fb1bb4efde29a8012073b067200d3c9e6b528887857"
    )
    expect_null(attr(x, "marker_source_license"))
    expect_equal(attr(x, "marker_repository_license"), "MIT")
    expect_equal(
      attr(x, "marker_repository_copyright"),
      "Copyright (c) 2022 Laboratory of RNA and Disease Data Science (RDDS)"
    )
    expect_equal(
      attr(x, "marker_repository_license_url"),
      paste0(
        "https://github.com/TebaldiLab/shiny_cellmarkeraccordion/blob/",
        "a2cc870a40df2cdd8f2c9671605b19e3f29229d7/LICENSE"
      )
    )
  }
})

test_that("the upstream marker-data license notice is installed", {
  notice <- system.file("COPYRIGHTS", package = "CellOnTools")
  expect_true(nzchar(notice))
  expect_true(file.exists(notice))

  text <- paste(readLines(notice, warn = FALSE), collapse = "\n")
  expect_match(text, "MIT License", fixed = TRUE)
  expect_match(
    text,
    "Copyright (c) 2022 Laboratory of RNA and Disease Data Science (RDDS)",
    fixed = TRUE
  )
  expect_match(
    text,
    "53ec885a4e3844c8493d3fb1bb4efde29a8012073b067200d3c9e6b528887857",
    fixed = TRUE
  )
})
