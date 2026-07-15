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
#'   If supplied, must be a named numeric vector covering \code{id1},
#'   \code{id2}, and every ancestor of either term.  A complete vector returned
#'   by \code{ontologySimilarity::descendants_IC()} satisfies this requirement.
#'   Values must be finite and non-negative, and names must be unique and
#'   non-empty.
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
    required_terms <- .required_ic_terms(c(id1, id2), clData)
    .validate_information_content(information_content, required_terms)
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

# Return the unique query terms and all ancestors required by
# ontologySimilarity::get_term_sim_mat().
.required_ic_terms <- function(ids, clData) {
  ancestors <- unlist(
    lapply(unique(ids), function(id) ontologyIndex::get_ancestors(clData, id)),
    use.names = FALSE
  )
  unique(c(ids, ancestors))
}

# Validate both the shape and term coverage of a pre-computed IC vector.
.validate_information_content <- function(information_content, required_terms) {
  if (!is.numeric(information_content) || !is.null(dim(information_content)) ||
      length(information_content) == 0L || is.null(names(information_content))) {
    stop("`information_content` must be a named numeric vector.", call. = FALSE)
  }

  ic_names <- names(information_content)
  invalid_names <- is.na(ic_names) | !nzchar(trimws(ic_names))
  if (any(invalid_names)) {
    stop("`information_content` names must not be NA, empty, or whitespace-only.",
         call. = FALSE)
  }
  if (anyDuplicated(ic_names)) {
    stop("`information_content` names must be unique.", call. = FALSE)
  }
  if (any(!is.finite(information_content))) {
    stop("`information_content` values must all be finite.", call. = FALSE)
  }
  if (any(information_content < 0)) {
    stop("`information_content` values must be non-negative.", call. = FALSE)
  }

  missing_ic <- required_terms[!required_terms %in% ic_names]
  if (length(missing_ic) > 0L) {
    stop(
      "`information_content` is missing IC for required term(s): ",
      paste(head(missing_ic, 3L), collapse = ", "),
      if (length(missing_ic) > 3L) {
        paste0(" (and ", length(missing_ic) - 3L, " more)")
      } else {
        ""
      },
      call. = FALSE
    )
  }

  invisible(TRUE)
}
