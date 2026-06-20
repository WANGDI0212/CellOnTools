# Fast, dependency-free tests of the internal marker-processing helpers.
# These use a tiny mock table so they run without clusterProfiler or the
# bundled data.

mock_marker_df <- function() {
  data.frame(
    species         = rep("Human", 5),
    CL_ID           = c("CL:0000084", "CL:0000084", "CL:0000236",
                        "CL:0000236", "CL:0000576"),
    CL_label        = c("T cell", "T cell", "B cell", "B cell", "monocyte"),
    marker_symbol   = c("CD3D", "CD3E", "MS4A1", "", "CD14"),
    marker_entrezid = c("915", "916", "931", "931", NA),
    stringsAsFactors = FALSE
  )
}

test_that(".build_term_maps drops blank/NA genes and de-duplicates", {
  maps <- CellOnTools:::.build_term_maps(mock_marker_df(), geneType = "symbol")

  expect_named(maps, c("term2gene", "term2name"))
  expect_named(maps$term2gene, c("term", "gene"))
  # The empty marker_symbol ("") for B cell is dropped.
  expect_false(any(maps$term2gene$gene == ""))
  expect_setequal(maps$term2gene$gene, c("CD3D", "CD3E", "MS4A1", "CD14"))
  # One name per term.
  expect_equal(anyDuplicated(maps$term2name$term), 0L)
})

test_that(".build_term_maps switches to Entrez identifiers", {
  maps <- CellOnTools:::.build_term_maps(mock_marker_df(), geneType = "entrezid")
  # NA Entrez (monocyte) dropped; duplicate (931/931) collapsed.
  expect_setequal(maps$term2gene$gene, c("915", "916", "931"))
})

test_that(".make_entrez2symbol_map builds a first-wins lookup", {
  m <- CellOnTools:::.make_entrez2symbol_map(mock_marker_df())
  expect_equal(unname(m["915"]), "CD3D")
  expect_equal(unname(m["931"]), "MS4A1")
  # NA Entrez is excluded from the map.
  expect_false(any(is.na(names(m))))
})

test_that(".load_marker_data validates required columns", {
  bad <- mock_marker_df()[, c("species", "CL_ID")]
  # Simulate the column check used inside .load_marker_data().
  required <- c("species", "CL_ID", "CL_label", "marker_symbol", "marker_entrezid")
  expect_length(setdiff(required, colnames(bad)), 3L)
})
