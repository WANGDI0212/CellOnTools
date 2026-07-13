#' Get Descendants of Cell Ontology Terms
#'
#' @description
#' Returns CL-namespace descendants of each queried term, optionally limited by
#' the number of graph edges (hops) below the query.
#'
#' @section Distance convention:
#' Hop distance is the shortest number of direct child edges from the query to
#' a descendant. It is deliberately distinct from the CL-only
#' \code{ancestor_count} returned by \code{\link{CLdepth}}.
#'
#' @param ids Character vector of CL IDs.  NA and empty strings are silently
#'   dropped.  Duplicates are preserved in the output list.
#' @param clData An \code{ontology_index} object returned by \code{CLload()}.
#' @param include_self Logical; if \code{TRUE}, include the query term itself
#'   in its own descendant list (default: \code{FALSE}).
#' @param max_hops \code{NULL} or a non-negative integer giving the maximum
#'   number of direct child edges to traverse (default: \code{NULL}, return all
#'   reachable CL descendants). A value of 0 returns no descendants unless
#'   \code{include_self = TRUE}; a value of 1 returns all direct CL children.
#'
#' @return Named list of character vectors.  Each element corresponds to one
#'   input ID and contains only \code{CL:*} IDs of its descendants. Returns
#'   \code{character(0)} for unknown IDs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clData <- CLload()
#'
#' # All descendants of lymphocyte
#' CLdescendants("CL:0000542", clData)
#'
#' # Include the query term itself
#' CLdescendants("CL:0000542", clData, include_self = TRUE)
#'
#' # Only descendants within 2 direct-child hops below the query
#' CLdescendants("CL:0000542", clData, max_hops = 2)
#'
#' # Multiple terms
#' CLdescendants(c("CL:0000084", "CL:0000236"), clData)
#' }
CLdescendants <- function(ids,
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

  # ---- Traverse direct CL child edges for each ID ----
  cl_ids <- .get_cl_ids(clData)
  result <- lapply(ids, function(id) {
    if (!grepl("^CL:\\d+$", id) || !id %in% clData$id) {
      return(character(0))
    }

    .walk_ontology(
      start = id,
      neighbours = clData$children,
      max_hops = max_hops,
      include_self = include_self,
      allowed_ids = cl_ids
    )
  })

  names(result) <- ids
  result
}
