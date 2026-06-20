# ============================================================================
# utils-markers.R
# Internal helpers for loading and processing CellMarkerAccordion data.
# All functions are unexported (prefixed with a dot).
# ============================================================================

# ----------------------------------------------------------------------------
# .load_marker_data
# Load the CellMarkerAccordion data object for the given species and validate
# its required columns.
# ----------------------------------------------------------------------------
.load_marker_data <- function(species = c("human", "mouse")) {
  species <- match.arg(species)

  obj_name <- switch(
    species,
    human = "CellMarkerAccordion_HumanHealthy",
    mouse = "CellMarkerAccordion_MouseHealthy"
  )

  marker_data <- tryCatch({
    utils::data(list = obj_name, package = "CellOnTools", envir = environment())
    get(obj_name, envir = environment(), inherits = FALSE)
  }, error = function(e) {
    stop("Failed to load marker data for species '", species, "'.\n",
         "Make sure the package data is properly installed.\n",
         "Error: ", e$message, call. = FALSE)
  })

  required_cols <- c("species", "CL_ID", "CL_label", "marker_symbol", "marker_entrezid")
  missing_cols  <- setdiff(required_cols, colnames(marker_data))
  if (length(missing_cols) > 0) {
    stop("Marker data is missing required columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  if (nrow(marker_data) == 0) {
    warning("Marker data for species '", species, "' is empty.", call. = FALSE)
  }

  marker_data
}

# ----------------------------------------------------------------------------
# .build_term_maps
# Build TERM2GENE and TERM2NAME data frames from marker_data.
#
# Parameters
#   marker_data : data frame from .load_marker_data()
#   geneType    : "symbol" or "entrezid"
#
# Returns a list with elements:
#   term2gene : data frame(term, gene)
#   term2name : data frame(term, name)
# ----------------------------------------------------------------------------
.build_term_maps <- function(marker_data, geneType = c("symbol", "entrezid")) {
  geneType <- match.arg(geneType)

  gene_col <- switch(geneType, symbol = "marker_symbol", entrezid = "marker_entrezid")

  term2gene <- marker_data[, c("CL_ID", gene_col), drop = FALSE]
  colnames(term2gene) <- c("term", "gene")
  term2gene <- term2gene[!is.na(term2gene$gene) & nzchar(term2gene$gene), , drop = FALSE]
  term2gene <- unique(term2gene)

  if (nrow(term2gene) == 0) {
    stop("No valid gene annotations found in marker data.", call. = FALSE)
  }

  term2name <- unique(marker_data[, c("CL_ID", "CL_label"), drop = FALSE])
  colnames(term2name) <- c("term", "name")
  term2name <- term2name[!is.na(term2name$name) & nzchar(term2name$name), , drop = FALSE]
  term2name <- term2name[!duplicated(term2name$term), , drop = FALSE]

  list(term2gene = term2gene, term2name = term2name)
}

# ----------------------------------------------------------------------------
# .make_entrez2symbol_map
# Build a named character vector mapping Entrez IDs to gene symbols.
# Duplicated Entrez IDs keep the first occurrence.
# ----------------------------------------------------------------------------
.make_entrez2symbol_map <- function(marker_data) {
  id_map <- unique(marker_data[, c("marker_entrezid", "marker_symbol"), drop = FALSE])
  id_map <- id_map[
    !is.na(id_map$marker_entrezid) & nzchar(id_map$marker_entrezid) &
    !is.na(id_map$marker_symbol)   & nzchar(id_map$marker_symbol), ,
    drop = FALSE
  ]
  id_map <- id_map[!duplicated(id_map$marker_entrezid), , drop = FALSE]
  stats::setNames(id_map$marker_symbol, id_map$marker_entrezid)
}

# ----------------------------------------------------------------------------
# .convert_result_gene_ids
# Replace "/" -separated Entrez IDs with gene symbols in enrichment result
# columns (geneID and/or core_enrichment).
#
# Parameters
#   result : enrichResult or compareClusterResult object
#   map    : named character vector (entrezid -> symbol) from .make_entrez2symbol_map()
#   slot   : "result" (enrichResult) or "compareClusterResult"
#   cols   : column names to convert (default: c("geneID", "core_enrichment"))
# ----------------------------------------------------------------------------
.convert_result_gene_ids <- function(result, map,
                                     slot = c("result", "compareClusterResult"),
                                     cols = c("geneID", "core_enrichment")) {
  slot <- match.arg(slot)

  df <- methods::slot(result, slot)

  .convert_col <- function(gene_str) {
    ids <- strsplit(gene_str, "/")[[1L]]
    syms <- ifelse(ids %in% names(map), map[ids], ids)
    paste(syms, collapse = "/")
  }

  for (col in intersect(cols, colnames(df))) {
    df[[col]] <- vapply(df[[col]], .convert_col, character(1L))
  }

  methods::slot(result, slot) <- df
  result
}
