#' Calculate Ancestor Count (Depth) of Cell Ontology Terms
#'
#' @description
#' Returns the **ancestor_count** for each queried CL term, defined as:
#'
#' \deqn{ancestor\_count(id) = |\{ancestors\}| - 1}
#'
#' where the ancestor set is obtained via
#' \code{ontologyIndex::get_ancestors(clData, id)} (which includes the term
#' itself), and subtracting 1 removes the term from its own count.
#'
#' @section Depth convention:
#' Throughout this package, **depth is synonymous with ancestor_count**.
#' It is \emph{not} the number of edges from the root, the shortest path, or
#' the longest path.  In a DAG ontology a term may have multiple paths to the
#' root; ancestor_count counts unique ancestors, not path length.
#'
#' Practical interpretation:
#' \itemize{
#'   \item Root term: ancestor_count = 0.
#'   \item Direct child of root: ancestor_count = 1.
#'   \item More specific (granular) terms have larger ancestor_counts.
#' }
#'
#' @param ids Character vector of CL IDs (e.g. \code{"CL:0000084"}).
#'   NA and empty strings are silently dropped.  Duplicates are preserved so
#'   that the output length always equals the number of valid input elements.
#' @param clData An \code{ontology_index} object returned by \code{CLload()}.
#'
#' @return Named integer vector of ancestor_counts, one per valid input ID.
#'   Returns \code{NA_integer_} for IDs that are not found in the ontology.
#'   Names are the input IDs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clData <- CLload()
#'
#' # Single term
#' CLdepth("CL:0000084", clData)   # T cell
#'
#' # Multiple terms - more specific terms have larger ancestor_count
#' CLdepth(c("CL:0000084", "CL:0000236", "CL:0000542"), clData)
#'
#' # Unknown IDs return NA with a warning
#' CLdepth(c("CL:0000084", "CL:9999999"), clData)
#' }
CLdepth <- function(ids, clData) {

  # ---- Validate inputs ----
  .validate_cldata(clData)

  if (!requireNamespace("ontologyIndex", quietly = TRUE)) {
    stop("Package 'ontologyIndex' is required. Install with:\n",
         "  BiocManager::install('ontologyIndex')", call. = FALSE)
  }

  ids <- .validate_ids(ids, clData = NULL, unique_only = FALSE,
                       allow_unknown = TRUE, warn_invalid = TRUE)

  # ---- Compute ancestor_count ----
  # For IDs not in the ontology, return NA and emit one aggregated warning.
  # Exclude bad-format IDs from the unknown warning: they were already reported
  # by .validate_ids(), so re-flagging them here would double-warn.
  known       <- ids %in% clData$id
  well_formed <- grepl("^CL:\\d+$", ids)
  unknown     <- unique(ids[well_formed & !known])

  if (length(unknown) > 0) {
    .warn_compact("Unknown CL ID(s) - returning NA", unknown)
  }

  out <- rep(NA_integer_, length(ids))
  names(out) <- ids

  if (any(known)) {
    out[known] <- .get_ancestor_count_vec(ids[known], clData)
  }

  out
}
