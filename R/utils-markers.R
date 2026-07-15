# ============================================================================
# utils-markers.R
# Internal helpers for loading and processing CellMarkerAccordion data.
# All functions are unexported (prefixed with a dot).
# ============================================================================

.MARKER_SOURCE_FILE <- "TheCellMarkerAccordion_database_v1.0.0.xlsx"
.MARKER_SOURCE_REPOSITORY <-
  "https://github.com/TebaldiLab/shiny_cellmarkeraccordion"
.MARKER_SOURCE_COMMIT <- "a2cc870a40df2cdd8f2c9671605b19e3f29229d7"
.MARKER_SOURCE_SHA256 <-
  "53ec885a4e3844c8493d3fb1bb4efde29a8012073b067200d3c9e6b528887857"
.MARKER_REPOSITORY_LICENSE <- "MIT"
.MARKER_REPOSITORY_COPYRIGHT <-
  "Copyright (c) 2022 Laboratory of RNA and Disease Data Science (RDDS)"
.MARKER_REPOSITORY_LICENSE_URL <- paste0(
  .MARKER_SOURCE_REPOSITORY,
  "/blob/", .MARKER_SOURCE_COMMIT, "/LICENSE"
)

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
    .get_marker_data_object(obj_name)
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

  release <- .normalise_cl_release(
    attr(marker_data, "ontology_release", exact = TRUE)
  )
  if (is.na(release) || !identical(release, .CL_RELEASE)) {
    stop(
      "Marker data for species '", species,
      "' is not harmonised to Cell Ontology release ", .CL_RELEASE, ".",
      call. = FALSE
    )
  }
  if (!identical(
    attr(marker_data, "ontology_url", exact = TRUE),
    .CL_RELEASE_URL
  ) || !identical(
    tolower(attr(marker_data, "ontology_md5", exact = TRUE)),
    .CL_RELEASE_MD5
  )) {
    stop(
      "Marker data for species '", species,
      "' has incomplete or inconsistent ontology provenance metadata.",
      call. = FALSE
    )
  }

  expected_source <- list(
    marker_source_file = .MARKER_SOURCE_FILE,
    marker_source_repository = .MARKER_SOURCE_REPOSITORY,
    marker_source_commit = .MARKER_SOURCE_COMMIT,
    marker_source_sha256 = .MARKER_SOURCE_SHA256,
    marker_repository_license = .MARKER_REPOSITORY_LICENSE,
    marker_repository_copyright = .MARKER_REPOSITORY_COPYRIGHT,
    marker_repository_license_url = .MARKER_REPOSITORY_LICENSE_URL
  )
  actual_source <- lapply(
    names(expected_source),
    function(name) attr(marker_data, name, exact = TRUE)
  )
  names(actual_source) <- names(expected_source)
  if (!identical(actual_source, expected_source)) {
    stop(
      "Marker data for species '", species,
      "' has incomplete or inconsistent source provenance metadata.",
      call. = FALSE
    )
  }

  invalid_ids <- is.na(marker_data$CL_ID) |
    !grepl("^CL:\\d+$", marker_data$CL_ID)
  if (any(invalid_ids)) {
    stop("Marker data contains invalid Cell Ontology IDs.", call. = FALSE)
  }
  obsolete_ids <- intersect(unique(marker_data$CL_ID),
                            names(.CL_MARKER_REPLACEMENTS))
  if (length(obsolete_ids) > 0L) {
    stop(
      "Marker data still contains obsolete Cell Ontology IDs: ",
      paste(obsolete_ids, collapse = ", "),
      call. = FALSE
    )
  }

  marker_data
}

.get_marker_data_object <- function(obj_name) {
  data_env <- new.env(parent = emptyenv())
  utils::data(
    list = obj_name,
    package = "CellOnTools",
    envir = data_env
  )
  get(obj_name, envir = data_env, inherits = FALSE)
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
# .validate_enrichment_parameters
# Validate arguments shared by CLenricher() and CLcompareCluster().
# ----------------------------------------------------------------------------
.validate_enrichment_parameters <- function(pvalueCutoff,
                                            pAdjustMethod,
                                            minGSSize,
                                            maxGSSize,
                                            qvalueCutoff,
                                            readable) {
  probabilities <- list(
    pvalueCutoff = pvalueCutoff,
    qvalueCutoff = qvalueCutoff
  )
  for (param_name in names(probabilities)) {
    value <- probabilities[[param_name]]
    if (!is.numeric(value) || length(value) != 1L ||
        !is.finite(value) || value < 0 || value > 1) {
      stop("`", param_name, "` must be a finite number in [0, 1].",
           call. = FALSE)
    }
  }

  if (!is.character(pAdjustMethod) || length(pAdjustMethod) != 1L ||
      is.na(pAdjustMethod) || !pAdjustMethod %in% stats::p.adjust.methods) {
    stop(
      "`pAdjustMethod` must be one of: ",
      paste(stats::p.adjust.methods, collapse = ", "), ".",
      call. = FALSE
    )
  }

  gene_set_sizes <- list(minGSSize = minGSSize, maxGSSize = maxGSSize)
  for (param_name in names(gene_set_sizes)) {
    value <- gene_set_sizes[[param_name]]
    if (!is.numeric(value) || length(value) != 1L ||
        !is.finite(value) || value < 1 || value != floor(value) ||
        value > .Machine$integer.max) {
      stop("`", param_name, "` must be a positive integer scalar.",
           call. = FALSE)
    }
  }
  if (minGSSize > maxGSSize) {
    stop("`minGSSize` (", minGSSize, ") must be <= `maxGSSize` (",
         maxGSSize, ").", call. = FALSE)
  }

  if (!is.logical(readable) || length(readable) != 1L || is.na(readable)) {
    stop("`readable` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

# ----------------------------------------------------------------------------
# .convert_result_gene_ids
# Make an enrichment result readable while preserving the S4 object contract.
# In addition to replacing "/"-separated Entrez IDs in the result table, this
# synchronises the metadata slots used by DOSE and clusterProfiler methods.
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

  required_slots <- c(slot, "gene2Symbol", "keytype", "readable")
  if (!all(required_slots %in% methods::slotNames(result))) {
    stop("Unsupported enrichment result object.", call. = FALSE)
  }
  if (!is.character(map) || is.null(names(map)) ||
      anyNA(names(map)) || any(!nzchar(names(map))) ||
      anyDuplicated(names(map))) {
    stop("`map` must be a named character vector with unique, non-empty names.",
         call. = FALSE)
  }

  df <- methods::slot(result, slot)

  .convert_col <- function(gene_str) {
    if (is.na(gene_str) || !nzchar(gene_str)) {
      return(gene_str)
    }
    ids <- strsplit(gene_str, "/", fixed = TRUE)[[1L]]
    syms <- unname(map[ids])
    syms[is.na(syms) | !nzchar(syms)] <- ids[is.na(syms) | !nzchar(syms)]
    paste(syms, collapse = "/")
  }

  for (col in intersect(cols, colnames(df))) {
    df[[col]] <- vapply(df[[col]], .convert_col, character(1L))
  }

  source_ids <- if (identical(slot, "result")) {
    result@gene
  } else {
    unname(unlist(result@geneClusters, use.names = FALSE))
  }
  source_ids <- unique(as.character(source_ids))
  source_ids <- source_ids[!is.na(source_ids) & nzchar(source_ids)]
  symbols <- unname(map[source_ids])
  unmapped <- is.na(symbols) | !nzchar(symbols)
  symbols[unmapped] <- source_ids[unmapped]

  methods::slot(result, slot) <- df
  result@gene2Symbol <- stats::setNames(symbols, source_ids)
  result@keytype <- "ENTREZID"
  result@readable <- TRUE
  result
}
