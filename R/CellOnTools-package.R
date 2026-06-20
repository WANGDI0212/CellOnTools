#' CellOnTools: Tools for Cell Ontology Analysis
#'
#' @description
#' CellOnTools provides utilities for mapping, querying, visualising, and
#' aggregating Cell Ontology (CL) terms, as well as marker-gene enrichment using
#' bundled healthy human and mouse CellMarkerAccordion annotations.
#'
#' @details
#' Main functionality includes:
#'
#' - Loading Cell Ontology data from AnnotationHub or local OBO files.
#' - Converting between CL identifiers and labels.
#' - Searching and mapping free-text cell type names to CL terms.
#' - Extracting and plotting ontology hierarchy subgraphs.
#' - Computing semantic similarity between CL terms.
#' - Rolling fine-grained CL terms up to coarser shared ancestors.
#' - Performing marker-gene enrichment against bundled marker annotations.
#'
#' @importFrom methods as
#' @importFrom utils download.file head
#' @keywords internal
"_PACKAGE"

utils::globalVariables(c("display_label", "is_query"))
