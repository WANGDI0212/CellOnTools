#' Load Cell Marker Accordion Data
#'
#' @description
#' Returns the CellMarkerAccordion marker gene table for the requested species.
#' This is a thin wrapper around the internal \code{.load_marker_data()} helper
#' that also validates the data structure and optionally checks for duplicate
#' CL_ID / gene combinations.
#'
#' The bundled tables integrate several source databases and therefore contain
#' repeated \code{CL_ID} / \code{marker_symbol} pairs by design; the enrichment
#' functions de-duplicate these internally.  The duplicate check is therefore
#' opt-in (\code{check_unique = TRUE}) rather than the default.
#'
#' @param species Species: \code{"human"} or \code{"mouse"} (default: \code{"human"}).
#' @param check_unique Logical; if \code{TRUE}, warn when duplicate
#'   CL_ID / marker_symbol pairs are present in the data (default: \code{FALSE}).
#'
#' @return Data frame with columns:
#' \describe{
#'   \item{\code{species}}{Species name.}
#'   \item{\code{CL_ID}}{Cell Ontology ID.}
#'   \item{\code{CL_label}}{Cell type name.}
#'   \item{\code{marker_symbol}}{Gene symbol.}
#'   \item{\code{marker_entrezid}}{Entrez Gene ID.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' human_markers <- CLmarkers("human")
#' head(human_markers)
#'
#' mouse_markers <- CLmarkers("mouse")
#'
#' # Markers for a specific cell type
#' t_cell_markers <- subset(human_markers, CL_label == "T cell")
#' print(t_cell_markers$marker_symbol)
#' }
CLmarkers <- function(species = c("human", "mouse"), check_unique = FALSE) {

  species <- match.arg(species)

  marker_data <- .load_marker_data(species)

  if (check_unique) {
    dup_key <- paste(marker_data$CL_ID, marker_data$marker_symbol, sep = "||")
    n_dup   <- sum(duplicated(dup_key))
    if (n_dup > 0L) {
      warning("Marker data contains ", n_dup,
              " duplicate CL_ID / marker_symbol pair(s).", call. = FALSE)
    }
  }

  marker_data
}
