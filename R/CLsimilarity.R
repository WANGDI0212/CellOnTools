#' Calculate Semantic Similarity Between Two Cell Ontology Terms
#'
#' @description
#' Computes the semantic similarity between two CL terms using either the
#' Resnik or Lin measure via \pkg{ontologySimilarity}.
#'
#' @param id1 A single CL ID (e.g. \code{"CL:0000084"}).
#' @param id2 A single CL ID (e.g. \code{"CL:0000236"}).
#' @param clData An \code{ontology_index} object returned by \code{CLload()}.
#' @param method Similarity measure: \code{"resnik"} (default) or \code{"lin"}.
#'   \itemize{
#'     \item \code{"resnik"}: IC of the most informative common ancestor;
#'       unbounded above, not normalised to the 0-to-1 interval.
#'     \item \code{"lin"}: Resnik similarity normalised by the sum of the two
#'       terms' ICs; typically between 0 and 1.
#'   }
#' @param information_content Pre-computed information content vector (default:
#'   \code{NULL}, auto-computed via \code{ontologySimilarity::descendants_IC()}).
#'   If supplied, must be a named numeric vector covering at least the common
#'   ancestors of \code{id1} and \code{id2}.
#'
#' @return A single numeric similarity score.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clData <- CLload()
#'
#' # Resnik similarity between T cell and B cell
#' CLsimilarity("CL:0000084", "CL:0000236", clData, method = "resnik")
#'
#' # Pre-compute IC for multiple comparisons (more efficient)
#' ic <- ontologySimilarity::descendants_IC(clData)
#' CLsimilarity("CL:0000084", "CL:0000236", clData, method = "lin",
#'              information_content = ic)
#' }
CLsimilarity <- function(id1,
                         id2,
                         clData,
                         method             = c("resnik", "lin"),
                         information_content = NULL) {

  method <- match.arg(method)

  # ---- Check dependencies ----
  if (!requireNamespace("ontologySimilarity", quietly = TRUE)) {
    stop("Package 'ontologySimilarity' is required. Install with:\n",
         "  install.packages('ontologySimilarity')", call. = FALSE)
  }

  # ---- Validate clData ----
  .validate_cldata(clData)

  # ---- Validate id1 / id2 ----
  if (missing(id1)) {
    stop("`id1` must be a single character CL ID.", call. = FALSE)
  }
  if (missing(id2)) {
    stop("`id2` must be a single character CL ID.", call. = FALSE)
  }

  .validate_one_id <- function(val, nm) {
    if (is.null(val) || !is.character(val) || length(val) != 1L || is.na(val)) {
      stop("`", nm, "` must be a single character CL ID.", call. = FALSE)
    }
    if (!grepl("^CL:\\d+$", val)) {
      stop("`", nm, "` has invalid CL ID format: ", val, call. = FALSE)
    }
    if (!val %in% clData$id) {
      stop("Unknown CL ID in `", nm, "`: ", val, call. = FALSE)
    }
    invisible(TRUE)
  }
  .validate_one_id(id1, "id1")
  .validate_one_id(id2, "id2")

  # ---- Prepare information content ----
  if (is.null(information_content)) {
    information_content <- ontologySimilarity::descendants_IC(clData)
  } else {
    if (!is.numeric(information_content) || is.null(names(information_content))) {
      stop("`information_content` must be a named numeric vector.", call. = FALSE)
    }
    # Check coverage of common ancestors (the nodes actually needed for the score)
    common_anc <- intersect(
      ontologyIndex::get_ancestors(clData, id1),
      ontologyIndex::get_ancestors(clData, id2)
    )
    missing_ic <- common_anc[!common_anc %in% names(information_content)]
    if (length(missing_ic) > 0L) {
      stop(
        "`information_content` is missing IC for required common ancestor(s): ",
        paste(head(missing_ic, 3L), collapse = ", "),
        if (length(missing_ic) > 3L) paste0(" (and ", length(missing_ic) - 3L, " more)") else "",
        call. = FALSE
      )
    }
  }

  # ---- Calculate similarity ----
  sim_mat <- ontologySimilarity::get_term_sim_mat(
    ontology            = clData,
    information_content = information_content,
    method              = method,
    row_terms           = id1,
    col_terms           = id2
  )

  as.numeric(sim_mat[1L, 1L])
}
