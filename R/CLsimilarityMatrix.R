#' Calculate Pairwise Semantic Similarity Matrix for Cell Ontology Terms
#'
#' @description
#' Computes a matrix of pairwise semantic similarities between two sets of CL
#' terms.  Input order and duplicates are preserved in the output matrix
#' dimensions.
#'
#' @param ids1 Character vector of CL IDs (row terms).  NA and empty strings
#'   are removed; duplicates are \strong{preserved} so that the output row
#'   count equals the number of valid elements in \code{ids1}.
#' @param ids2 Character vector of CL IDs (column terms, default: same as
#'   \code{ids1}).  Same duplicate-preservation rule applies.
#' @param clData An \code{ontology_index} object returned by \code{CLload()}.
#' @param method Similarity measure: \code{"resnik"} (default) or \code{"lin"}.
#'   See \code{\link{CLsimilarity}} for details.
#' @param information_content Pre-computed information content vector (default:
#'   \code{NULL}, auto-computed).  If supplied, it must be a named numeric
#'   vector covering every term in \code{ids1} and \code{ids2}, together with
#'   all ancestors of those terms.  A complete vector returned by
#'   \code{ontologySimilarity::descendants_IC()} satisfies this requirement.
#'   Values must be finite and non-negative, and names must be unique and
#'   non-empty.
#' @param verbose Logical; if \code{TRUE} (default), print progress messages.
#'
#' @return Numeric matrix with \code{length(ids1)} rows and \code{length(ids2)}
#'   columns.  Row and column names are formatted as \code{"CL:XXXXXXX (label)"}.
#'   If \code{ids1} or \code{ids2} contain duplicates, the corresponding rows or
#'   columns will have identical names - use \code{make.unique()} if unique names
#'   are required downstream.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clData <- CLload()
#'
#' cell_types <- c("CL:0000084", "CL:0000236", "CL:0000542")
#' sim_mat <- CLsimilarityMatrix(cell_types, clData = clData)
#'
#' # Visualize
#' library(pheatmap)
#' pheatmap(sim_mat)
#'
#' # Cross-comparison
#' sim_cross <- CLsimilarityMatrix(
#'   ids1 = c("CL:0000084", "CL:0000236"),
#'   ids2 = c("CL:0000542", "CL:0000576"),
#'   clData = clData
#' )
#' }
CLsimilarityMatrix <- function(ids1,
                               ids2               = NULL,
                               clData,
                               method             = c("resnik", "lin"),
                               information_content = NULL,
                               verbose            = TRUE) {

  method <- match.arg(method)

  # ---- Check dependencies ----
  if (!requireNamespace("ontologySimilarity", quietly = TRUE)) {
    stop("Package 'ontologySimilarity' is required. Install with:\n",
         "  install.packages('ontologySimilarity')", call. = FALSE)
  }

  # ---- Validate clData ----
  .validate_cldata(clData)

  # ---- Validate ids1 ----
  if (missing(ids1) || is.null(ids1) || length(ids1) == 0L) {
    stop("`ids1` must be a non-empty character vector of CL IDs.", call. = FALSE)
  }
  ids1 <- as.character(ids1)
  ids1 <- ids1[!is.na(ids1) & nzchar(ids1)]   # drop NA/empty, preserve duplicates
  if (length(ids1) == 0L) {
    stop("No valid IDs in `ids1` after removing NA and empty strings.", call. = FALSE)
  }

  # Validate format and existence (use unique for checking, not for computing)
  bad1 <- unique(ids1[!grepl("^CL:\\d+$", ids1)])
  if (length(bad1) > 0L) {
    stop("Invalid CL ID format in `ids1`: ",
         paste(head(bad1, 3L), collapse = ", "), call. = FALSE)
  }
  unk1 <- unique(ids1[!ids1 %in% clData$id])
  if (length(unk1) > 0L) {
    stop("Unknown CL IDs in `ids1`: ",
         paste(head(unk1, 3L), collapse = ", "), call. = FALSE)
  }

  # ---- Validate ids2 ----
  symmetric <- is.null(ids2)
  if (symmetric) {
    ids2 <- ids1
  } else {
    ids2 <- as.character(ids2)
    ids2 <- ids2[!is.na(ids2) & nzchar(ids2)]
    if (length(ids2) == 0L) {
      stop("No valid IDs in `ids2` after removing NA and empty strings.", call. = FALSE)
    }
    bad2 <- unique(ids2[!grepl("^CL:\\d+$", ids2)])
    if (length(bad2) > 0L) {
      stop("Invalid CL ID format in `ids2`: ",
           paste(head(bad2, 3L), collapse = ", "), call. = FALSE)
    }
    unk2 <- unique(ids2[!ids2 %in% clData$id])
    if (length(unk2) > 0L) {
      stop("Unknown CL IDs in `ids2`: ",
           paste(head(unk2, 3L), collapse = ", "), call. = FALSE)
    }
  }

  # ---- Prepare information content ----
  if (is.null(information_content)) {
    if (verbose) message("Computing information content...")
    information_content <- ontologySimilarity::descendants_IC(clData)
  } else {
    required_terms <- .required_ic_terms(c(ids1, ids2), clData)
    .validate_information_content(information_content, required_terms)
  }

  # ---- Compute similarity matrix ----
  # ontologySimilarity::get_term_sim_mat() requires unique row/col terms.
  # We compute on unique IDs and then expand back to preserve duplicates.
  if (verbose) {
    message("Calculating similarity matrix...")
    message("  Method:      ", method)
    message("  Row terms:   ", length(ids1),
            if (length(ids1) != length(unique(ids1)))
              paste0(" (", length(unique(ids1)), " unique)") else "")
    message("  Column terms:", length(ids2),
            if (length(ids2) != length(unique(ids2)))
              paste0(" (", length(unique(ids2)), " unique)") else "")
  }

  unique_ids1 <- unique(ids1)
  unique_ids2 <- unique(ids2)

  sim_unique <- ontologySimilarity::get_term_sim_mat(
    ontology            = clData,
    information_content = information_content,
    method              = method,
    row_terms           = unique_ids1,
    col_terms           = unique_ids2
  )

  # Expand back to original (possibly duplicated) order
  row_idx <- match(ids1, unique_ids1)
  col_idx <- match(ids2, unique_ids2)
  sim_mat <- sim_unique[row_idx, col_idx, drop = FALSE]

  # ---- Assign row/column names ----
  make_names <- function(ids) paste0(ids, " (", clData$name[ids], ")")
  rownames(sim_mat) <- make_names(ids1)
  colnames(sim_mat) <- make_names(ids2)

  if (verbose) message("Similarity matrix computed successfully.")

  sim_mat
}
