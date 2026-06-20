# Integration tests for the enrichment wrappers.  These exercise the real
# bundled marker data through clusterProfiler and are skipped when the
# (Suggested) package is unavailable.

test_that("CLenricher returns an enrichResult for T-cell markers", {
  testthat::skip_if_not_installed("clusterProfiler")

  res <- suppressMessages(
    CLenricher(c("CD3D", "CD3E", "CD8A", "CD8B", "CD4", "IL7R", "CCR7"),
               geneType = "symbol", species = "human")
  )
  expect_s4_class(res, "enrichResult")
  df <- as.data.frame(res)
  expect_gt(nrow(df), 0L)
  # The most enriched terms should be T-cell related.
  expect_true(any(grepl("T cell", utils::head(df$Description, 5L),
                        ignore.case = TRUE)))
})

test_that("CLenricher converts Entrez IDs to symbols when readable = TRUE", {
  testthat::skip_if_not_installed("clusterProfiler")

  res <- suppressMessages(
    CLenricher(c("915", "916", "925", "926", "920"),
               geneType = "entrezid", species = "human", readable = TRUE)
  )
  testthat::skip_if(is.null(res) || nrow(as.data.frame(res)) == 0L,
                    "no enrichment for this gene set")
  gene_ids <- as.data.frame(res)$geneID
  # After conversion the geneID column holds symbols, not bare Entrez numbers.
  expect_false(any(grepl("^[0-9]+(/[0-9]+)*$", gene_ids)))
})

test_that("CLenricher validates its inputs", {
  testthat::skip_if_not_installed("clusterProfiler")

  expect_error(CLenricher(character(0), species = "human"),
               "non-empty character vector")
  expect_error(
    suppressMessages(CLenricher("CD3D", species = "human", pvalueCutoff = 2)),
    "\\[0, 1\\]"
  )
  expect_error(
    suppressMessages(CLenricher("CD3D", species = "human",
                                minGSSize = 100, maxGSSize = 10)),
    "must be <="
  )
})

test_that("CLcompareCluster compares multiple gene clusters", {
  testthat::skip_if_not_installed("clusterProfiler")

  clusters <- list(
    Tcell = c("CD3D", "CD3E", "CD8A", "CD4", "IL7R"),
    Bcell = c("MS4A1", "CD79A", "CD19", "CD79B"),
    Mono  = c("LYZ", "CST3", "CD14", "FCN1")
  )
  res <- suppressMessages(
    CLcompareCluster(clusters, geneType = "symbol", species = "human")
  )
  expect_s4_class(res, "compareClusterResult")
  df <- as.data.frame(res)
  expect_setequal(levels(df$Cluster), c("Tcell", "Bcell", "Mono"))
})

test_that("CLcompareCluster errors on empty clusters unless told to drop them", {
  testthat::skip_if_not_installed("clusterProfiler")

  clusters <- list(Tcell = c("CD3D", "CD3E"), Empty = character(0))
  expect_error(
    suppressMessages(CLcompareCluster(clusters, species = "human")),
    "no valid genes"
  )
})
