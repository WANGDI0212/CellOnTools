#' Map Cell Type Names to Cell Ontology Terms
#'
#' @description
#' Maps free-text cell type names to Cell Ontology terms via the OLS
#' (Ontology Lookup Service) API.  Identical queries (after normalisation) are
#' searched only once; results are mapped back to all occurrences in the input.
#'
#' @section Local reranking:
#' OLS results are locally reranked before returning, using the following
#' priority order:
#' \enumerate{
#'   \item Exact label match (case-insensitive).
#'   \item Normalised exact match (after separator/case normalisation).
#'   \item Label starts with the query.
#'   \item Label contains the query as a substring.
#'   \item Original OLS rank.
#' }
#'
#' @param query Character vector of cell type names or descriptions.
#' @param returnType Type of result to return:
#'   \itemize{
#'     \item \code{"all"} (default): data frame with full information.
#'     \item \code{"id"}: named character vector of CL IDs.
#'     \item \code{"label"}: named character vector of CL labels.
#'   }
#'   When \code{max_results > 1}, only \code{"all"} is supported.
#' @param max_results Maximum number of candidate matches per query (default:
#'   \code{1}).  If \code{> 1}, \code{returnType} must be \code{"all"}.
#' @param verbose Logical; if \code{TRUE} (default), print progress and summary.
#'
#' @return Depends on \code{returnType} and \code{max_results}:
#'   \itemize{
#'     \item \code{"all"}: data frame with columns \code{query_original},
#'       \code{query_display}, \code{query_actual}, \code{cl_label},
#'       \code{cl_id}, \code{match_status}, \code{error_message}, and
#'       (when \code{max_results > 1}) \code{rank}.
#'     \item \code{"id"} / \code{"label"}: named character vector.
#'   }
#'   Returns \code{NA} for unmatched, errored, or invalid queries.
#'   Input order and duplicates are preserved.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Map one cell type name and return the full result table
#' t_cell <- CLmap("T cell")
#' t_cell
#'
#' # Return only the best-matching CL ID for each query
#' cell_ids <- CLmap(
#'   c("T cell", "B cell", "classical monocyte"),
#'   returnType = "id",
#'   verbose = FALSE
#' )
#' cell_ids
#'
#' # Inspect multiple ranked candidates per query
#' candidates <- CLmap(
#'   c("helper T cell", "macrophage"),
#'   max_results = 5,
#'   verbose = FALSE
#' )
#' candidates
#'
#' # Duplicate queries are searched once and mapped back to all inputs
#' duplicate_example <- CLmap(
#'   c("T cells", "T cell", "B cells", NA, ""),
#'   returnType = "all",
#'   verbose = TRUE
#' )
#' duplicate_example
#'
#' # Return labels instead of IDs
#' cell_labels <- CLmap(
#'   c("CD8 positive T cell", "plasma cell"),
#'   returnType = "label",
#'   verbose = FALSE
#' )
#' cell_labels
#' }
CLmap <- function(query,
                  returnType  = c("all", "id", "label"),
                  max_results = 1L,
                  verbose     = TRUE) {

  # ========================================================================
  # Private helpers
  # ========================================================================

  .normalize_query <- function(q) {
    q <- tolower(gsub("[_/\\-]+", " ", q))
    q <- gsub("\\bcells\\b", "cell", q)
    q <- gsub("\\s+", " ", q)
    trimws(q)
  }

  # Local reranking: exact > normalised-exact > startsWith > contains > OLS order
  .rerank <- function(df_cl, query_actual) {
    if (nrow(df_cl) <= 1L) return(df_cl)
    lbl_lower <- tolower(df_cl$label)
    q_lower   <- tolower(query_actual)
    score <- ifelse(lbl_lower == q_lower,                          1L,
             ifelse(.normalize_query(lbl_lower) == q_lower,        2L,
             ifelse(startsWith(lbl_lower, q_lower),                3L,
             ifelse(grepl(q_lower, lbl_lower, fixed = TRUE),       4L,
                                                                   5L))))
    df_cl[order(score, seq_len(nrow(df_cl))), , drop = FALSE]
  }

  .empty_row <- function(q_orig, q_display, q_actual, status, error = NA_character_,
                         rank = 1L) {
    data.frame(query_original = q_orig, query_display = q_display,
               query_actual = q_actual, cl_label = NA_character_,
               cl_id = NA_character_, match_status = status,
               error_message = error, rank = as.integer(rank),
               stringsAsFactors = FALSE)
  }

  .search_one <- function(q_display, q_actual, max_results, verbose) {
    tryCatch({
      obj <- rols::OlsSearch(q = q_actual, ontology = "cl", type = "class",
                             groupField = TRUE, obsoletes = FALSE)
      df  <- as(rols::olsSearch(obj), "data.frame")

      if (!all(c("label", "obo_id") %in% colnames(df))) {
        stop("OLS result missing required columns: label, obo_id.")
      }

      df_cl <- df[grep("^CL:", df$obo_id), c("label", "obo_id"), drop = FALSE]
      df_cl <- df_cl[!duplicated(df_cl[, c("label", "obo_id")]), , drop = FALSE]

      if (nrow(df_cl) == 0L) {
        return(list(status = "no_match", error = NA_character_, data = NULL))
      }

      df_cl <- .rerank(df_cl, q_actual)
      df_cl <- df_cl[seq_len(min(nrow(df_cl), max_results)), , drop = FALSE]

      list(
        status = "matched",
        error  = NA_character_,
        data   = data.frame(
          query_display = rep(q_display, nrow(df_cl)),
          query_actual  = rep(q_actual,  nrow(df_cl)),
          cl_label      = df_cl$label,
          cl_id         = df_cl$obo_id,
          rank          = seq_len(nrow(df_cl)),
          stringsAsFactors = FALSE
        )
      )
    }, error = function(e) {
      if (verbose) warning("Search failed for '", q_actual, "': ",
                           conditionMessage(e), call. = FALSE)
      list(status = "api_error", error = conditionMessage(e), data = NULL)
    })
  }

  # ========================================================================
  # Argument validation
  # ========================================================================

  returnType <- match.arg(returnType)

  if (missing(query) || is.null(query) || length(query) == 0L) {
    stop("`query` must be a non-empty character vector.", call. = FALSE)
  }
  query_original_all <- as.character(query)
  n_input <- length(query_original_all)

  if (!is.numeric(max_results) || length(max_results) != 1L ||
      is.na(max_results) || max_results < 1L) {
    stop("`max_results` must be a positive integer.", call. = FALSE)
  }
  max_results <- as.integer(max_results)

  if (max_results > 1L && returnType != "all") {
    stop("When max_results > 1, returnType must be 'all'.", call. = FALSE)
  }
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!requireNamespace("rols", quietly = TRUE)) {
    stop("Package 'rols' is required. Install with: BiocManager::install('rols')",
         call. = FALSE)
  }

  # ========================================================================
  # Identify valid queries and normalise
  # ========================================================================

  valid_idx <- !is.na(query_original_all) & nzchar(trimws(query_original_all))
  n_valid   <- sum(valid_idx)

  if (verbose) {
    message("Processing ", n_input, " input quer",
            ifelse(n_input == 1L, "y", "ies"), "...")
    if (any(!valid_idx))
      message("  Note: ", sum(!valid_idx), " NA/empty quer",
              ifelse(sum(!valid_idx) == 1L, "y", "ies"),
              " will return invalid_input")
  }

  query_display_all <- query_original_all
  query_actual_all  <- rep(NA_character_, n_input)
  if (n_valid > 0L) {
    query_actual_all[valid_idx] <- .normalize_query(query_original_all[valid_idx])
  }

  # ========================================================================
  # Deduplicate for efficient searching
  # ========================================================================

  query_actual_unique <- unique(query_actual_all[valid_idx])
  n_unique <- length(query_actual_unique)

  rep_idx     <- match(query_actual_unique, query_actual_all)
  rep_display <- query_display_all[rep_idx]
  names(rep_display) <- query_actual_unique

  if (verbose) {
    message("Deduplicated to ", n_unique, " unique normalised quer",
            ifelse(n_unique == 1L, "y", "ies"), " for searching")
    message("\nSearching Cell Ontology via OLS...")
  }

  # ========================================================================
  # Search
  # ========================================================================

  search_results <- stats::setNames(vector("list", n_unique), query_actual_unique)

  for (i in seq_along(query_actual_unique)) {
    q_actual  <- query_actual_unique[i]
    q_display <- rep_display[[q_actual]]
    if (verbose) message("  [", i, "/", n_unique, "] Searching: ", q_actual)
    search_results[[q_actual]] <- .search_one(q_display, q_actual, max_results, verbose)
  }

  # ========================================================================
  # Map results back to original queries
  # ========================================================================

  if (verbose) message("\nMapping results back to original queries...")

  results_list <- lapply(seq_len(n_input), function(i) {
    q_orig    <- query_original_all[i]
    q_display <- query_display_all[i]
    q_actual  <- query_actual_all[i]

    if (!valid_idx[i]) {
      return(.empty_row(q_orig, q_display, NA_character_, "invalid_input"))
    }

    sr <- search_results[[q_actual]]
    if (is.null(sr)) {
      return(.empty_row(q_orig, q_display, q_actual, "api_error",
                        "Internal error: missing cached result."))
    }

    if (identical(sr$status, "matched") && !is.null(sr$data) && nrow(sr$data) > 0L) {
      return(do.call(rbind, lapply(seq_len(nrow(sr$data)), function(j) {
        data.frame(query_original = q_orig, query_display = q_display,
                   query_actual = q_actual, cl_label = sr$data$cl_label[j],
                   cl_id = sr$data$cl_id[j], match_status = "matched",
                   error_message = NA_character_, rank = as.integer(sr$data$rank[j]),
                   stringsAsFactors = FALSE)
      })))
    }

    .empty_row(q_orig, q_display, q_actual, sr$status, sr$error)
  })

  df <- do.call(rbind, results_list)
  rownames(df) <- NULL

  # ========================================================================
  # Summary
  # ========================================================================

  if (verbose) {
    df_r1       <- df[df$rank == 1L, , drop = FALSE]
    n_matched   <- sum(df_r1$match_status == "matched")
    n_no_match  <- sum(df_r1$match_status == "no_match")
    n_api_error <- sum(df_r1$match_status == "api_error")
    n_invalid   <- sum(df_r1$match_status == "invalid_input")
    match_rate  <- if (n_valid > 0L) n_matched / n_valid * 100 else 0

    message("\n", strrep("=", 68))
    message("Mapping Summary")
    message(strrep("=", 68))
    message("Total input queries:        ", n_input)
    message("Valid queries:              ", n_valid)
    message("Unique searches performed:  ", n_unique)
    message("Matched:                    ", n_matched,
            " (", sprintf("%.1f%%", match_rate), " of valid)")
    message("No match:                   ", n_no_match)
    message("API error:                  ", n_api_error)
    message("Invalid input:              ", n_invalid)
    if (max_results > 1L) {
      message("Total candidates returned:  ",
              sum(df$match_status == "matched" & !is.na(df$cl_id)))
    }
    if (n_valid > 0L && n_unique < n_valid) {
      message("\nNote: ", n_valid - n_unique, " duplicate quer",
              ifelse(n_valid - n_unique == 1L, "y was", "ies were"),
              " searched once and mapped to all occurrences.")
    }
    message(strrep("=", 68), "\n")
  }

  # ========================================================================
  # Return
  # ========================================================================

  if (max_results == 1L) {
    df$rank <- NULL
    if (returnType == "id")    return(stats::setNames(df$cl_id,    df$query_original))
    if (returnType == "label") return(stats::setNames(df$cl_label, df$query_original))
  }

  df
}
