#' Get Descendants of Cell Ontology Terms
#'
#' @description
#' Returns all descendants of each queried CL term, optionally filtered by how
#' far below the query term they are (measured in ancestor_count difference).
#'
#' @section Depth convention:
#' Throughout this package, **depth is synonymous with ancestor_count**:
#'
#' \deqn{ancestor\_count(id) = |\{ancestors\}| - 1}
#'
#' It is \emph{not} the number of edges from the root.  See \code{\link{CLdepth}}
#' for a full explanation.
#'
#' @param ids Character vector of CL IDs.  NA and empty strings are silently
#'   dropped.  Duplicates are preserved in the output list.
#' @param clData An \code{ontology_index} object returned by \code{CLload()}.
#' @param include_self Logical; if \code{TRUE}, include the query term itself
#'   in its own descendant list (default: \code{FALSE}).
#' @param max_descendant_ancestor_count Maximum allowed ancestor_count difference
#'   between a returned descendant and the query term (default: \code{NULL},
#'   return all descendants).  Formally, a descendant \eqn{d} is retained when:
#'   \deqn{ancestor\_count(d) - ancestor\_count(query) \le max\_descendant\_ancestor\_count}
#'   A value of 1 returns only direct children; larger values return progressively
#'   more distant descendants.
#'
#' @return Named list of character vectors.  Each element corresponds to one
#'   input ID and contains the CL IDs of its descendants.  Returns
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
#' # Only descendants within 2 ancestor_count steps below the query
#' CLdescendants("CL:0000542", clData, max_descendant_ancestor_count = 2)
#'
#' # Multiple terms
#' CLdescendants(c("CL:0000084", "CL:0000236"), clData)
#' }
CLdescendants <- function(ids,
                          clData,
                          include_self                  = FALSE,
                          max_descendant_ancestor_count = NULL) {

  # ---- Validate inputs ----
  .validate_cldata(clData)

  if (!requireNamespace("ontologyIndex", quietly = TRUE)) {
    stop("Package 'ontologyIndex' is required. Install with:\n",
         "  BiocManager::install('ontologyIndex')", call. = FALSE)
  }

  ids <- .validate_ids(ids, clData = NULL, unique_only = FALSE,
                       allow_unknown = TRUE, warn_invalid = TRUE)

  if (!is.null(max_descendant_ancestor_count)) {
    if (!is.numeric(max_descendant_ancestor_count) ||
        length(max_descendant_ancestor_count) != 1L ||
        is.na(max_descendant_ancestor_count) ||
        max_descendant_ancestor_count < 0) {
      stop("`max_descendant_ancestor_count` must be NULL or a non-negative number.",
           call. = FALSE)
    }
    max_descendant_ancestor_count <- as.integer(max_descendant_ancestor_count)
  }

  # ---- Warn once for all unknown IDs ----
  # Exclude bad-format IDs: they were already reported by .validate_ids(), so
  # re-flagging them here as "unknown" would double-warn for the same value.
  well_formed <- grepl("^CL:\\d+$", ids)
  unknown <- unique(ids[well_formed & !ids %in% clData$id])
  if (length(unknown) > 0) {
    .warn_compact("Unknown CL ID(s) - returning empty vector", unknown)
  }

  # ---- Get descendants for each ID ----
  result <- lapply(ids, function(id) {
    if (!id %in% clData$id) return(character(0))

    desc <- ontologyIndex::get_descendants(clData, id)

    if (!include_self) desc <- setdiff(desc, id)

    # Apply ancestor_count filter
    if (!is.null(max_descendant_ancestor_count) && length(desc) > 0) {
      query_count <- .get_ancestor_count_one(id, clData)
      desc_counts <- .get_ancestor_count_vec(desc, clData)
      # Keep descendants where the difference in ancestor_count is within the limit
      desc <- desc[desc_counts - query_count <= max_descendant_ancestor_count]
    }

    desc
  })

  names(result) <- ids
  result
}
