#' Healthy human CellMarkerAccordion marker annotations
#'
#' @description
#' A marker-gene annotation table linking healthy human cell types to Cell
#' Ontology (CL) identifiers and their marker genes.  The table is the data
#' source for [CLmarkers()], [CLenricher()] and [CLcompareCluster()].
#'
#' @format A data frame with 140,337 rows and 5 variables:
#' \describe{
#'   \item{species}{Species label (\code{"Human"}).}
#'   \item{CL_ID}{Cell Ontology identifier (e.g. \code{"CL:0000084"}).}
#'   \item{CL_label}{Cell Ontology label (cell type name).}
#'   \item{marker_symbol}{Gene symbol.}
#'   \item{marker_entrezid}{Entrez Gene identifier (character;
#'     \code{NA} where no Entrez mapping is available).}
#' }
#'
#' @details
#' Each row is one (cell type, marker gene) association restricted to the
#' healthy-tissue subset of the source resource.  Cell types are harmonised to
#' Cell Ontology terms so that the annotations can be combined directly with the
#' ontology-based operations in this package.
#'
#' @source Derived from the CellMarkerAccordion marker-gene resource (healthy
#'   human collection), filtered to harmonised Cell Ontology terms and bundled
#'   with CellOnTools.  See the package README for the full citation of the
#'   CellMarkerAccordion resource.
#'
#' @seealso [CLmarkers()], [CLenricher()], [CLcompareCluster()]
#' @keywords datasets
"CellMarkerAccordion_HumanHealthy"

#' Healthy mouse CellMarkerAccordion marker annotations
#'
#' @description
#' A marker-gene annotation table linking healthy mouse cell types to Cell
#' Ontology (CL) identifiers and their marker genes.  The table is the data
#' source for [CLmarkers()], [CLenricher()] and [CLcompareCluster()].
#'
#' @format A data frame with 49,289 rows and 5 variables:
#' \describe{
#'   \item{species}{Species label (\code{"Mouse"}).}
#'   \item{CL_ID}{Cell Ontology identifier (e.g. \code{"CL:0000084"}).}
#'   \item{CL_label}{Cell Ontology label (cell type name).}
#'   \item{marker_symbol}{Gene symbol.}
#'   \item{marker_entrezid}{Entrez Gene identifier (character;
#'     \code{NA} where no Entrez mapping is available).}
#' }
#'
#' @details
#' Each row is one (cell type, marker gene) association restricted to the
#' healthy-tissue subset of the source resource.  Cell types are harmonised to
#' Cell Ontology terms so that the annotations can be combined directly with the
#' ontology-based operations in this package.
#'
#' @source Derived from the CellMarkerAccordion marker-gene resource (healthy
#'   mouse collection), filtered to harmonised Cell Ontology terms and bundled
#'   with CellOnTools.  See the package README for the full citation of the
#'   CellMarkerAccordion resource.
#'
#' @seealso [CLmarkers()], [CLenricher()], [CLcompareCluster()]
#' @keywords datasets
"CellMarkerAccordion_MouseHealthy"
