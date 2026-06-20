#' Find Common Ancestor(s) of Cell Ontology Terms
#'
#' @description
#' Returns the set of CL terms that are ancestors of every query term.
#' When \code{most_specific = TRUE}, only the common ancestors with the
#' largest ancestor_count are returned (i.e. the most granular shared nodes).
#'
#' @section Depth convention:
#' Throughout this package, **depth is synonymous with ancestor_count**:
#'
#' \deqn{ancestor\_count(id) = |\{ancestors\}| - 1}
#'
#' "Most specific" means largest ancestor_count, not deepest in any path sense.
#' See \code{\link{CLdepth}} for a full explanation.
#'
#' @param ids Character vector of CL IDs.  After removing NA and empty strings,
#'   duplicates are silently dropped (set semantics).
#' @param clData An \code{ontology_index} object returned by \code{CLload()}.
#' @param most_specific Logical; if \code{TRUE}, return only the common
#'   ancestors with the maximum ancestor_count (default: \code{FALSE}).
#'
#' @return Character vector of common ancestor CL IDs.  Returns
#'   \code{character(0)} if no common ancestors exist or if any ID is unknown.
#'   If a single valid ID is provided, that ID itself is returned (with a
#'   warning).
#'
#' @details
#' Because the Cell Ontology is a Directed Acyclic Graph (DAG), a term may
#' have multiple parents.  When \code{most_specific = TRUE}, more than one
#' "most specific" common ancestor can be returned if multiple nodes share the
#' same maximum ancestor_count and are all ancestors of every query term.
#' These nodes are incomparable to each other (neither is an ancestor of the
#' other) yet are both maximally specific.  This is the correct behaviour for
#' a DAG.
#'
#' If you need a single unambiguous result, take the first element of the
#' returned vector or apply additional domain-specific filtering.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clData <- CLload()
#'
#' # All common ancestors of T cell and B cell
#' CLcommonAncestor(c("CL:0000084", "CL:0000236"), clData)
#'
#' # Most specific (largest ancestor_count) common ancestor(s)
#' # Note: may return more than one term in a DAG ontology
#' CLcommonAncestor(c("CL:0000084", "CL:0000236"), clData, most_specific = TRUE)
#' }
CLcommonAncestor <- function(ids,
                             clData,
                             most_specific = FALSE) {

  # ---- Validate inputs ----
  .validate_cldata(clData)

  if (!requireNamespace("ontologyIndex", quietly = TRUE)) {
    stop("Package 'ontologyIndex' is required. Install with:\n",
         "  BiocManager::install('ontologyIndex')", call. = FALSE)
  }

  ids <- .validate_ids(ids, clData = NULL, unique_only = TRUE,
                       allow_unknown = TRUE, warn_invalid = TRUE)

  if (length(ids) == 1L) {
    warning("Only one unique ID provided. Returning the ID itself.", call. = FALSE)
    return(ids)
  }

  # ---- Check for unknown IDs ----
  unknown <- ids[!ids %in% clData$id]
  if (length(unknown) > 0) {
    .warn_compact("Unknown CL ID(s) - returning character(0)", unknown)
    return(character(0))
  }

  # ---- Find common ancestors ----
  # get_ancestors() includes the term itself, so the intersection naturally
  # includes any query term that is an ancestor of all others.
  all_ancestors <- lapply(ids, function(id) ontologyIndex::get_ancestors(clData, id))
  common <- Reduce(intersect, all_ancestors)

  if (length(common) == 0L) return(character(0))

  # ---- Filter to most specific (largest ancestor_count) if requested ----
  if (most_specific && length(common) > 1L) {
    counts    <- .get_ancestor_count_vec(common, clData)
    max_count <- max(counts)
    common    <- common[counts == max_count]
  }

  common
}
