#' Search Cell Ontology by Label
#'
#' @description
#' Searches Cell Ontology term labels for one or more patterns and returns a
#' data frame of matching terms.
#'
#' By default, patterns are treated as **literal strings** (not regular
#' expressions).  Set \code{use_regex = TRUE} to enable regex matching.
#'
#' @param pattern Character vector of search patterns.  NA and empty strings
#'   are silently dropped.
#' @param clData An \code{ontology_index} object returned by \code{CLload()}.
#' @param ignore_case Logical; if \code{TRUE} (default), perform
#'   case-insensitive matching.
#' @param exact_match Logical; if \code{TRUE}, require the full label to equal
#'   the pattern (default: \code{FALSE}, partial/substring match).
#' @param use_regex Logical; if \code{TRUE}, treat \code{pattern} as a regular
#'   expression; if \code{FALSE} (default), treat it as a literal string.
#'   Ignored when \code{exact_match = TRUE}.
#' @param max_results Maximum number of results to return per pattern
#'   (default: \code{NULL}, return all matches).  Must be a positive integer
#'   if specified.
#'
#' @return Data frame with columns \code{pattern}, \code{id}, \code{label},
#'   \code{search_mode}.  Returns zero rows (with a warning) if no matches are
#'   found.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clData <- CLload()
#'
#' # Literal substring search (default)
#' CLsearchLabel("T cell", clData)
#'
#' # Case-insensitive literal search
#' CLsearchLabel("lymph", clData, ignore_case = TRUE)
#'
#' # Exact match
#' CLsearchLabel("T cell", clData, exact_match = TRUE)
#'
#' # Regex search
#' CLsearchLabel("^CD[48]", clData, use_regex = TRUE, ignore_case = TRUE)
#'
#' # Limit results per pattern
#' CLsearchLabel("cell", clData, max_results = 5)
#'
#' # Multiple patterns
#' CLsearchLabel(c("T cell", "B cell", "macrophage"), clData, max_results = 5)
#' }
CLsearchLabel <- function(pattern,
                          clData,
                          ignore_case = TRUE,
                          exact_match = FALSE,
                          use_regex   = FALSE,
                          max_results = NULL) {

  # ---- Validate clData ----
  .validate_cldata(clData)

  # ---- Validate pattern ----
  if (missing(pattern) || is.null(pattern) || length(pattern) == 0L) {
    stop("`pattern` must be a non-empty character vector.", call. = FALSE)
  }
  pattern <- as.character(pattern)
  pattern <- pattern[!is.na(pattern) & nzchar(pattern)]
  if (length(pattern) == 0L) {
    stop("No valid patterns after removing NA and empty strings.", call. = FALSE)
  }

  # ---- Validate max_results ----
  if (!is.null(max_results)) {
    if (!is.numeric(max_results) || length(max_results) != 1L ||
        is.na(max_results) || max_results < 1) {
      stop("`max_results` must be NULL or a positive integer.", call. = FALSE)
    }
    max_results <- as.integer(max_results)
  }

  # ---- Search for each pattern ----
  all_results <- lapply(pattern, function(pat) {

    if (exact_match) {
      matches <- if (ignore_case) {
        which(tolower(clData$name) == tolower(pat))
      } else {
        which(clData$name == pat)
      }
      search_mode <- "exact"
    } else {
      # Partial match: literal (fixed = TRUE) or regex (fixed = FALSE)
      if (use_regex) {
        matches <- grep(pat, clData$name, ignore.case = ignore_case)
      } else if (ignore_case) {
        matches <- grep(tolower(pat), tolower(clData$name), fixed = TRUE)
      } else {
        matches <- grep(pat, clData$name, fixed = TRUE)
      }
      search_mode <- if (use_regex) "regex" else "partial"
    }

    if (length(matches) == 0L) {
      return(data.frame(
        pattern    = character(0),
        id         = character(0),
        label      = character(0),
        search_mode = character(0),
        stringsAsFactors = FALSE
      ))
    }

    if (!is.null(max_results) && length(matches) > max_results) {
      matches <- matches[seq_len(max_results)]
    }

    data.frame(
      pattern    = pat,
      id         = clData$id[matches],
      label      = clData$name[matches],
      search_mode = search_mode,
      stringsAsFactors = FALSE
    )
  })

  results <- do.call(rbind, all_results)
  rownames(results) <- NULL

  if (nrow(results) == 0L) {
    .warn_compact("No matches found for pattern(s)", pattern)
  }

  results
}
