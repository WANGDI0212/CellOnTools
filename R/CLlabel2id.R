#' Convert Cell Ontology Labels to IDs
#'
#' @description
#' Maps cell type label strings to their CL IDs.  Exact matching is used by
#' default; case-insensitive matching is available via \code{ignore_case = TRUE}.
#'
#' @param labels Character vector of cell type labels.
#'   NA and empty strings are returned as \code{NA_character_} without warning.
#'   Input order and duplicates are preserved.
#' @param clData An \code{ontology_index} object returned by \code{CLload()}.
#' @param strict Logical; if \code{TRUE}, stop on the first unknown label
#'   instead of returning \code{NA} (default: \code{FALSE}).
#' @param ignore_case Logical; if \code{TRUE}, perform case-insensitive matching
#'   (default: \code{FALSE}).  When enabled and two ontology labels differ only
#'   in case, a warning is emitted and the first match (by ontology order) is
#'   returned.
#'
#' @return Character vector of CL IDs, same length as \code{labels}.
#'   Names are preserved from the input if present.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clData <- CLload()
#'
#' # Exact match
#' CLlabel2id("T cell", clData)   # "CL:0000084"
#'
#' # Case-insensitive
#' CLlabel2id("t cell", clData, ignore_case = TRUE)
#'
#' # Multiple labels
#' CLlabel2id(c("T cell", "B cell", "lymphocyte"), clData)
#'
#' # Unknown labels return NA with aggregated warning
#' CLlabel2id(c("T cell", "Unknown cell type"), clData)
#' }
CLlabel2id <- function(labels, clData, strict = FALSE, ignore_case = FALSE) {

  # ---- Validate clData ----
  .validate_cldata(clData)

  # ---- Validate labels ----
  if (missing(labels) || is.null(labels)) {
    stop("`labels` must be provided.", call. = FALSE)
  }
  if (length(labels) == 0L) return(character(0))

  original_names <- names(labels)
  labels <- as.character(labels)

  # ---- Build lookup ----
  # CL OBO files can include imported BFO/CARO/etc. terms.  This function is a
  # Cell Ontology lookup, so imported labels must never resolve to non-CL IDs.
  is_cl <- grepl("^CL:\\d+$", clData$id)
  cl_ids <- unname(clData$id[is_cl])
  cl_names <- unname(clData$name[is_cl])
  label2id <- stats::setNames(cl_ids, cl_names)

  # ---- Case-insensitive collision check ----
  if (ignore_case) {
    lower_names <- tolower(cl_names)
    dup_lower   <- duplicated(lower_names) | duplicated(lower_names, fromLast = TRUE)
    if (any(dup_lower)) {
      colliding <- unique(cl_names[dup_lower])
      .warn_compact(
        "Case-insensitive collision: multiple ontology labels map to the same lowercase form (first match used)",
        colliding
      )
    }
    # Build case-insensitive map (first occurrence wins)
    label2id_lower <- stats::setNames(cl_ids, lower_names)
    label2id_lower <- label2id_lower[!duplicated(names(label2id_lower))]
  }

  # ---- Classify labels ----
  is_na_or_empty <- is.na(labels) | !nzchar(labels)

  # Exact match
  exact_hit <- !is_na_or_empty & (labels %in% names(label2id))

  # Case-insensitive match (only for those not already exact-matched)
  ci_hit <- rep(FALSE, length(labels))
  if (ignore_case) {
    ci_hit <- !is_na_or_empty & !exact_hit & (tolower(labels) %in% names(label2id_lower))
  }

  unknown <- !is_na_or_empty & !exact_hit & !ci_hit

  # ---- Aggregated warning / stop for unknowns ----
  if (any(unknown)) {
    msg <- paste0("Unknown cell type label(s) (returning NA): ",
                  paste(head(unique(labels[unknown]), 3L), collapse = ", "),
                  if (sum(unknown) > 3L) paste0(" (and ", sum(unknown) - 3L, " more)") else "")
    if (strict) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }

  # ---- Build result ----
  result <- rep(NA_character_, length(labels))
  result[exact_hit] <- unname(label2id[labels[exact_hit]])
  if (ignore_case && any(ci_hit)) {
    result[ci_hit] <- unname(label2id_lower[tolower(labels[ci_hit])])
  }

  if (!is.null(original_names)) names(result) <- original_names

  result
}
