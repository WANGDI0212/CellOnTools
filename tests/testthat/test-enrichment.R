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
  # Assert biological content without depending on clusterProfiler's tie/order
  # behaviour across releases.
  expect_true(any(grepl("T cell", df$Description, ignore.case = TRUE)))
  expect_true(all(is.finite(df$p.adjust) & df$p.adjust >= 0 & df$p.adjust <= 1))
})

test_that("CLenricher converts Entrez IDs to symbols when readable = TRUE", {
  testthat::skip_if_not_installed("clusterProfiler")

  res <- suppressMessages(
    CLenricher(c("915", "916", "925", "926", "920"),
               geneType = "entrezid", species = "human", readable = TRUE,
               pvalueCutoff = 1, qvalueCutoff = 1)
  )
  expect_s4_class(res, "enrichResult")
  expect_gt(nrow(as.data.frame(res)), 0L)
  gene_ids <- as.data.frame(res)$geneID
  # After conversion the geneID column holds symbols, not bare Entrez numbers.
  expect_false(any(grepl("^[0-9]+(/[0-9]+)*$", gene_ids)))
  # Keep the metadata slots in sync with the readable result table so that
  # downstream DOSE/clusterProfiler methods do not try to convert it again.
  expect_true(res@readable)
  expect_identical(res@keytype, "ENTREZID")
  expect_true(all(res@gene %in% names(res@gene2Symbol)))
  expect_identical(unname(res@gene2Symbol["915"]), "CD3D")
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
  expect_error(CLenricher("CD3D", minGSSize = 1.5),
               "positive integer scalar")
  expect_error(CLenricher("CD3D", pAdjustMethod = "not-a-method"),
               "pAdjustMethod")
  expect_error(CLenricher("CD3D", readable = NA),
               "readable.*TRUE or FALSE")
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

test_that("CLcompareCluster keeps readable S4 metadata internally consistent", {
  testthat::skip_if_not_installed("clusterProfiler")

  clusters <- list(
    Tcell = c("915", "916", "925", "926", "920"),
    Myeloid = c("929", "933", "942", "945")
  )
  res <- suppressMessages(
    CLcompareCluster(
      clusters,
      geneType = "entrezid",
      species = "human",
      readable = TRUE,
      pvalueCutoff = 1,
      qvalueCutoff = 1
    )
  )

  expect_s4_class(res, "compareClusterResult")
  expect_gt(nrow(as.data.frame(res)), 0L)
  expect_true(res@readable)
  expect_identical(res@keytype, "ENTREZID")
  expect_true(all(unlist(res@geneClusters, use.names = FALSE) %in%
                    names(res@gene2Symbol)))
  expect_false(any(grepl("^[0-9]+(/[0-9]+)*$", as.data.frame(res)$geneID)))
})

test_that("CLcompareCluster rejects ambiguous cluster names", {
  testthat::skip_if_not_installed("clusterProfiler")

  clusters <- list("CD3D", "MS4A1")

  names(clusters) <- c("A", NA_character_)
  expect_error(CLcompareCluster(clusters), "must not be NA")

  names(clusters) <- c("A", "A")
  expect_error(CLcompareCluster(clusters), "must be unique")

  names(clusters) <- c("A", "   ")
  expect_error(CLcompareCluster(clusters), "whitespace-only")
})

test_that("CLcompareCluster errors on empty clusters unless told to drop them", {
  testthat::skip_if_not_installed("clusterProfiler")

  clusters <- list(Tcell = c("CD3D", "CD3E"), Empty = character(0))
  expect_error(
    suppressMessages(CLcompareCluster(clusters, species = "human")),
    "no valid genes"
  )
  expect_error(CLcompareCluster(list(A = "CD3D"), drop_empty_clusters = NA),
               "drop_empty_clusters.*TRUE or FALSE")
})
