#' Convert Cell Ontology IDs to Labels
#'
#' @description
#' Maps CL IDs to their human-readable cell type names.  Invalid-format and
#' unknown IDs return \code{NA} (with a single aggregated warning per problem
#' class) unless \code{strict = TRUE}.
#'
#' @param ids Character vector of CL IDs (e.g. \code{"CL:0000084"}).
#'   NA and empty strings are returned as \code{NA_character_} without warning.
#'   Input order and duplicates are preserved.
#' @param clData An \code{ontology_index} object returned by \code{CLload()}.
#' @param strict Logical; if \code{TRUE}, stop on the first invalid or unknown
#'   ID instead of returning \code{NA} (default: \code{FALSE}).
#'
#' @return Character vector of cell type labels, same length as \code{ids}.
#'   Names are preserved from the input if present.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clData <- CLload()
#'
#' # Single ID
#' CLid2label("CL:0000084", clData)   # "T cell"
#'
#' # Multiple IDs - order and duplicates preserved
#' CLid2label(c("CL:0000084", "CL:0000236", "CL:0000084"), clData)
#'
#' # Mixed valid / invalid - returns NA with aggregated warnings
#' CLid2label(c("CL:0000084", "CL:9999999", "not_an_id"), clData)
#'
#' # Named input preserves names
#' CLid2label(c(s1 = "CL:0000084", s2 = "CL:0000236"), clData)
#' }
CLid2label <- function(ids, clData, strict = FALSE) {

  # ---- Validate clData ----
  .validate_cldata(clData)

  # ---- Validate ids ----
  if (missing(ids) || is.null(ids)) {
    stop("`ids` must be provided.", call. = FALSE)
  }
  if (length(ids) == 0L) return(character(0))

  original_names <- names(ids)
  ids <- as.character(ids)

  # ---- Build lookup ----
  id2label <- stats::setNames(clData$name, clData$id)

  # ---- Classify IDs ----
  is_na_or_empty <- is.na(ids) | !nzchar(ids)
  bad_fmt  <- !is_na_or_empty & !grepl("^CL:\\d+$", ids)
  unknown  <- !is_na_or_empty & !bad_fmt & !(ids %in% names(id2label))

  # ---- Aggregated warnings (or stop in strict mode) ----
  if (any(bad_fmt)) {
    msg <- paste0("Invalid CL ID format (returning NA): ",
                  paste(head(unique(ids[bad_fmt]), 3L), collapse = ", "),
                  if (sum(bad_fmt) > 3L) paste0(" (and ", sum(bad_fmt) - 3L, " more)") else "")
    if (strict) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }
  if (any(unknown)) {
    msg <- paste0("Unknown CL ID(s) (returning NA): ",
                  paste(head(unique(ids[unknown]), 3L), collapse = ", "),
                  if (sum(unknown) > 3L) paste0(" (and ", sum(unknown) - 3L, " more)") else "")
    if (strict) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }

  # ---- Build result ----
  result <- rep(NA_character_, length(ids))
  valid  <- !is_na_or_empty & !bad_fmt & !unknown
  result[valid] <- unname(id2label[ids[valid]])

  if (!is.null(original_names)) names(result) <- original_names

  result
}
