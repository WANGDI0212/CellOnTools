#' Compare Cell Ontology Enrichment Across Multiple Gene Clusters
#'
#' @description
#' Runs \code{clusterProfiler::compareCluster()} using Cell Ontology marker
#' gene annotations, comparing enrichment across multiple gene sets.
#'
#' @param geneClusters Named list of gene vectors.  Names are cluster IDs;
#'   values are character vectors of gene symbols or Entrez IDs.  Each cluster
#'   is cleaned (NA, empty strings, and duplicates removed) before analysis.
#' @param geneType Type of gene identifiers: \code{"symbol"} or \code{"entrezid"}.
#' @param species Species: \code{"human"} or \code{"mouse"}.
#' @param pvalueCutoff P-value cutoff (default: \code{0.05}). Must be between
#'   0 and 1.
#' @param pAdjustMethod P-value adjustment method (default: \code{"BH"}).
#' @param universe Background gene set (default: \code{NULL}, use all genes in
#'   the marker annotation).
#' @param minGSSize Minimum number of annotated marker genes a cell type must
#'   have to be tested (default: \code{1}).
#' @param maxGSSize Maximum number of annotated marker genes a cell type may
#'   have to be tested (default: \code{10000}).  Must be \code{>= minGSSize}.
#' @param qvalueCutoff Q-value cutoff (default: \code{0.2}). Must be between
#'   0 and 1.
#' @param readable Logical; if \code{TRUE} and \code{geneType = "entrezid"},
#'   convert Entrez IDs to gene symbols in the result (default: \code{FALSE}).
#' @param drop_empty_clusters Logical; if \code{FALSE} (default), stop when a
#'   cluster has no valid genes.  If \code{TRUE}, silently drop empty clusters
#'   and continue.
#'
#' @return A \code{compareClusterResult} object from \pkg{clusterProfiler}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cluster_markers <- list(
#'   Cluster1 = c("CD3D", "CD3E", "CD8A"),
#'   Cluster2 = c("MS4A1", "CD79A", "CD19"),
#'   Cluster3 = c("LYZ", "CST3", "CD14")
#' )
#'
#' result <- CLcompareCluster(cluster_markers, geneType = "symbol", species = "human")
#'
#' library(enrichplot)
#' dotplot(result)
#' }
CLcompareCluster <- function(geneClusters,
                             geneType           = c("symbol", "entrezid"),
                             species            = c("human", "mouse"),
                             pvalueCutoff       = 0.05,
                             pAdjustMethod      = "BH",
                             universe           = NULL,
                             minGSSize          = 1L,
                             maxGSSize          = 10000L,
                             qvalueCutoff       = 0.2,
                             readable           = FALSE,
                             drop_empty_clusters = FALSE) {

  # ---- Argument matching ----
  geneType <- match.arg(geneType)
  species  <- match.arg(species)

  # ---- Check dependencies ----
  if (!requireNamespace("clusterProfiler", quietly = TRUE)) {
    stop("Package 'clusterProfiler' is required. Install with:\n",
         "  BiocManager::install('clusterProfiler')", call. = FALSE)
  }

  # ---- Validate geneClusters ----
  if (missing(geneClusters) || !is.list(geneClusters) || length(geneClusters) == 0L) {
    stop("`geneClusters` must be a non-empty named list of gene vectors.", call. = FALSE)
  }
  cluster_names <- names(geneClusters)
  if (is.null(cluster_names)) {
    stop("`geneClusters` must be a named list with non-empty names.", call. = FALSE)
  }
  invalid_names <- is.na(cluster_names) | !nzchar(trimws(cluster_names))
  if (any(invalid_names)) {
    stop("`geneClusters` names must not be NA, empty, or whitespace-only.",
         call. = FALSE)
  }
  if (anyDuplicated(cluster_names)) {
    duplicate_names <- unique(cluster_names[duplicated(cluster_names)])
    stop("`geneClusters` names must be unique; duplicated name(s): ",
         paste(duplicate_names, collapse = ", "), ".", call. = FALSE)
  }

  # ---- Validate analysis parameters ----
  .validate_enrichment_parameters(
    pvalueCutoff = pvalueCutoff,
    pAdjustMethod = pAdjustMethod,
    minGSSize = minGSSize,
    maxGSSize = maxGSSize,
    qvalueCutoff = qvalueCutoff,
    readable = readable
  )
  if (!is.logical(drop_empty_clusters) ||
      length(drop_empty_clusters) != 1L || is.na(drop_empty_clusters)) {
    stop("`drop_empty_clusters` must be TRUE or FALSE.", call. = FALSE)
  }

  # Clean each cluster
  geneClusters <- lapply(geneClusters, function(x) {
    x <- as.character(x)
    unique(x[!is.na(x) & nzchar(x)])
  })

  cluster_sizes <- lengths(geneClusters)
  empty_clusters <- names(geneClusters)[cluster_sizes == 0L]

  if (length(empty_clusters) > 0L) {
    if (drop_empty_clusters) {
      .warn_compact("Dropping empty cluster(s)", empty_clusters)
      geneClusters <- geneClusters[cluster_sizes > 0L]
      if (length(geneClusters) == 0L) {
        stop("No clusters remain after dropping empty ones.", call. = FALSE)
      }
    } else {
      stop("The following cluster(s) have no valid genes: ",
           paste(empty_clusters, collapse = ", "),
           "\nSet drop_empty_clusters = TRUE to skip them.", call. = FALSE)
    }
  }

  # ---- Load marker data and build term maps ----
  marker_data <- .load_marker_data(species)
  maps        <- .build_term_maps(marker_data, geneType = geneType)

  # ---- Prepare universe ----
  if (is.null(universe)) {
    universe <- unique(maps$term2gene$gene)
  } else {
    universe <- unique(as.character(universe[!is.na(universe) & nzchar(universe)]))
  }

  # ---- Informative messages ----
  message("Running CL enrichment comparison across ",
          length(geneClusters), " cluster(s)...")
  message("  Gene type:        ", geneType)
  message("  Species:          ", species)
  message("  Universe size:    ", length(universe))
  message("  Cell types in DB: ", length(unique(maps$term2gene$term)))

  for (cl_name in names(geneClusters)) {
    n_mapped <- sum(geneClusters[[cl_name]] %in% maps$term2gene$gene)
    message("  ", cl_name, ": ", length(geneClusters[[cl_name]]),
            " gene(s), ", n_mapped, " mapped")
  }

  # ---- Define per-cluster enrichment function ----
  cl_enrich_fun <- function(gene) {
    clusterProfiler::enricher(
      gene          = gene,
      pvalueCutoff  = pvalueCutoff,
      pAdjustMethod = pAdjustMethod,
      universe      = universe,
      minGSSize     = minGSSize,
      maxGSSize     = maxGSSize,
      qvalueCutoff  = qvalueCutoff,
      TERM2GENE     = maps$term2gene,
      TERM2NAME     = maps$term2name
    )
  }

  # ---- Run compareCluster ----
  result <- clusterProfiler::compareCluster(
    geneClusters = geneClusters,
    fun          = cl_enrich_fun
  )

  # ---- Convert to readable ----
  if (readable && geneType == "entrezid" &&
      !is.null(result) && nrow(result@compareClusterResult) > 0L) {
    entrez2symbol <- .make_entrez2symbol_map(marker_data)
    result <- .convert_result_gene_ids(result, entrez2symbol,
                                       slot = "compareClusterResult")
    message("Converted Entrez IDs to gene symbols.")
  }

  # ---- Summary ----
  if (!is.null(result) && nrow(result@compareClusterResult) > 0L) {
    n_sig  <- nrow(result@compareClusterResult)
    n_hits <- length(unique(result@compareClusterResult$Cluster))
    message("Comparison completed:")
    message("  Total significant enrichments: ", n_sig)
    message("  Clusters with enrichments:     ", n_hits, " / ", length(geneClusters))
  } else {
    message("No significant enrichment found in any cluster.")
  }

  result
}
