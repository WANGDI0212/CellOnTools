#' Get Ancestors of Cell Ontology Terms
#'
#' @description
#' Returns all ancestors of each queried CL term, optionally filtered by how
#' far above the query term they are (measured in ancestor_count difference).
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
#'   in its own ancestor list (default: \code{FALSE}).
#' @param max_ancestor_count Maximum allowed ancestor_count difference between
#'   the query term and a returned ancestor (default: \code{NULL}, return all
#'   ancestors).  Formally, an ancestor \eqn{a} is retained when:
#'   \deqn{ancestor\_count(query) - ancestor\_count(a) \le max\_ancestor\_count}
#'   A value of 1 returns only direct parents; larger values return progressively
#'   more distant ancestors.
#'
#' @return Named list of character vectors.  Each element corresponds to one
#'   input ID and contains the CL IDs of its ancestors.  Returns
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
#' # Only ancestors within 3 ancestor_count steps above the query
#' CLancestors("CL:0000084", clData, max_ancestor_count = 3)
#'
#' # Multiple terms
#' CLancestors(c("CL:0000084", "CL:0000236"), clData)
#' }
CLancestors <- function(ids,
                        clData,
                        include_self      = FALSE,
                        max_ancestor_count = NULL) {

  # ---- Validate inputs ----
  .validate_cldata(clData)

  if (!requireNamespace("ontologyIndex", quietly = TRUE)) {
    stop("Package 'ontologyIndex' is required. Install with:\n",
         "  BiocManager::install('ontologyIndex')", call. = FALSE)
  }

  ids <- .validate_ids(ids, clData = NULL, unique_only = FALSE,
                       allow_unknown = TRUE, warn_invalid = TRUE)

  if (!is.null(max_ancestor_count)) {
    if (!is.numeric(max_ancestor_count) || length(max_ancestor_count) != 1L ||
        is.na(max_ancestor_count) || max_ancestor_count < 0) {
      stop("`max_ancestor_count` must be NULL or a non-negative number.", call. = FALSE)
    }
    max_ancestor_count <- as.integer(max_ancestor_count)
  }

  # ---- Warn once for all unknown IDs ----
  # Exclude bad-format IDs: they were already reported by .validate_ids(), so
  # re-flagging them here as "unknown" would double-warn for the same value.
  well_formed <- grepl("^CL:\\d+$", ids)
  unknown <- unique(ids[well_formed & !ids %in% clData$id])
  if (length(unknown) > 0) {
    .warn_compact("Unknown CL ID(s) - returning empty vector", unknown)
  }

  # ---- Get ancestors for each ID ----
  result <- lapply(ids, function(id) {
    if (!id %in% clData$id) return(character(0))

    anc <- ontologyIndex::get_ancestors(clData, id)

    if (!include_self) anc <- setdiff(anc, id)

    # Apply ancestor_count filter
    if (!is.null(max_ancestor_count) && length(anc) > 0) {
      query_count <- .get_ancestor_count_one(id, clData)
      anc_counts  <- .get_ancestor_count_vec(anc, clData)
      # Keep ancestors where the difference in ancestor_count is within the limit
      anc <- anc[query_count - anc_counts <= max_ancestor_count]
    }

    anc
  })

  names(result) <- ids
  result
}
