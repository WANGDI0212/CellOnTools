#' Cell Ontology Marker Gene Enrichment Analysis
#'
#' @description
#' Tests a gene set for over-representation of Cell Ontology cell-type marker
#' genes using \code{clusterProfiler::enricher()}.
#'
#' @param gene Character vector of gene symbols or Entrez IDs.
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
#'
#' @return An \code{enrichResult} object from \pkg{clusterProfiler}, or
#'   \code{NULL} if no significant enrichment is found.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Gene symbols
#' result <- CLenricher(c("CD3D", "CD3E", "CD8A", "CD8B"),
#'                      geneType = "symbol", species = "human")
#' library(enrichplot)
#' dotplot(result)
#'
#' # Entrez IDs with readable output
#' result <- CLenricher(c("915", "916", "925", "926"),
#'                      geneType = "entrezid", species = "human", readable = TRUE)
#' }
CLenricher <- function(gene,
                       geneType      = c("symbol", "entrezid"),
                       species       = c("human", "mouse"),
                       pvalueCutoff  = 0.05,
                       pAdjustMethod = "BH",
                       universe      = NULL,
                       minGSSize     = 1L,
                       maxGSSize     = 10000L,
                       qvalueCutoff  = 0.2,
                       readable      = FALSE) {

  # ---- Argument matching ----
  geneType <- match.arg(geneType)
  species  <- match.arg(species)

  # ---- Check dependencies ----
  if (!requireNamespace("clusterProfiler", quietly = TRUE)) {
    stop("Package 'clusterProfiler' is required. Install with:\n",
         "  BiocManager::install('clusterProfiler')", call. = FALSE)
  }

  # ---- Validate gene input ----
  if (missing(gene) || length(gene) == 0L) {
    stop("`gene` must be a non-empty character vector.", call. = FALSE)
  }
  gene <- as.character(gene)
  gene_raw <- gene
  gene <- unique(gene[!is.na(gene) & nzchar(gene)])
  if (length(gene) == 0L) {
    stop("No valid genes after removing NA and empty strings.", call. = FALSE)
  }
  n_dedup <- length(gene_raw) - length(gene)

  # ---- Validate analysis parameters ----
  .validate_enrichment_parameters(
    pvalueCutoff = pvalueCutoff,
    pAdjustMethod = pAdjustMethod,
    minGSSize = minGSSize,
    maxGSSize = maxGSSize,
    qvalueCutoff = qvalueCutoff,
    readable = readable
  )

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
  n_mapped <- sum(gene %in% maps$term2gene$gene)
  message("Running CL enrichment analysis...")
  message("  Input genes:          ", length(gene_raw))
  if (n_dedup > 0L) message("  Deduplicated:         ", n_dedup, " duplicate(s) removed")
  message("  Gene type:            ", geneType)
  message("  Species:              ", species)
  message("  Mapped to universe:   ", n_mapped, " / ", length(gene))
  message("  Universe size:        ", length(universe))
  message("  Cell types in DB:     ", length(unique(maps$term2gene$term)))

  # ---- Run enrichment ----
  result <- clusterProfiler::enricher(
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

  # ---- Convert to readable ----
  if (readable && geneType == "entrezid" &&
      !is.null(result) && nrow(result@result) > 0L) {
    entrez2symbol <- .make_entrez2symbol_map(marker_data)
    result <- .convert_result_gene_ids(result, entrez2symbol, slot = "result")
    message("Converted Entrez IDs to gene symbols.")
  }

  # ---- Summary ----
  if (!is.null(result) && nrow(result@result) > 0L) {
    message("Enrichment completed: ", nrow(result@result),
            " significant cell type(s) found.")
  } else {
    message("No significant enrichment found.")
  }

  result
}
