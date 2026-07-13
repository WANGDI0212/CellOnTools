#' Get Ancestors of Cell Ontology Terms
#'
#' @description
#' Returns CL-namespace ancestors of each queried term, optionally limited by
#' the number of graph edges (hops) above the query.
#'
#' @section Distance convention:
#' Hop distance is the shortest number of direct parent edges from the query
#' to an ancestor. It is deliberately distinct from the CL-only
#' \code{ancestor_count} returned by \code{\link{CLdepth}}.
#'
#' @param ids Character vector of CL IDs.  NA and empty strings are silently
#'   dropped.  Duplicates are preserved in the output list.
#' @param clData An \code{ontology_index} object returned by \code{CLload()}.
#' @param include_self Logical; if \code{TRUE}, include the query term itself
#'   in its own ancestor list (default: \code{FALSE}).
#' @param max_hops \code{NULL} or a non-negative integer giving the maximum
#'   number of direct parent edges to traverse (default: \code{NULL}, return all
#'   reachable CL ancestors). A value of 0 returns no ancestors unless
#'   \code{include_self = TRUE}; a value of 1 returns all direct CL parents.
#'
#' @return Named list of character vectors.  Each element corresponds to one
#'   input ID and contains only \code{CL:*} IDs of its ancestors. Returns
#'   \code{character(0)} for unknown IDs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clData <- CLload()
#'
#' # All ancestors of T cell
#' CLancestors("CL:0000084", clData)
#'
#' # Include the query term itself
#' CLancestors("CL:0000084", clData, include_self = TRUE)
#'
#' # Only ancestors within 3 direct-parent hops above the query
#' CLancestors("CL:0000084", clData, max_hops = 3)
#'
#' # Multiple terms
#' CLancestors(c("CL:0000084", "CL:0000236"), clData)
#' }
CLancestors <- function(ids,
                        clData,
                        include_self = FALSE,
                        max_hops = NULL) {

  # ---- Validate inputs ----
  .validate_cldata(clData)

  if (!requireNamespace("ontologyIndex", quietly = TRUE)) {
    stop("Package 'ontologyIndex' is required. Install with:\n",
         "  BiocManager::install('ontologyIndex')", call. = FALSE)
  }

  ids <- .validate_ids(ids, clData = NULL, unique_only = FALSE,
                       allow_unknown = TRUE, warn_invalid = TRUE)

  include_self <- .validate_logical_scalar(include_self, "include_self")
  max_hops <- .validate_integer_scalar(
    max_hops, "max_hops", minimum = 0L, null_ok = TRUE
  )

  # ---- Warn once for all unknown IDs ----
  # Exclude bad-format IDs: they were already reported by .validate_ids(), so
  # re-flagging them here as "unknown" would double-warn for the same value.
  well_formed <- grepl("^CL:\\d+$", ids)
  unknown <- unique(ids[well_formed & !ids %in% clData$id])
  if (length(unknown) > 0) {
    .warn_compact("Unknown CL ID(s) - returning empty vector", unknown)
  }

  # ---- Traverse direct CL parent edges for each ID ----
  cl_ids <- .get_cl_ids(clData)
  result <- lapply(ids, function(id) {
    if (!grepl("^CL:\\d+$", id) || !id %in% clData$id) {
      return(character(0))
    }

    .walk_ontology(
      start = id,
      neighbours = clData$parents,
      max_hops = max_hops,
      include_self = include_self,
      allowed_ids = cl_ids
    )
  })

  names(result) <- ids
  result
}
