#' Interactively Map Cell Type Names to Cell Ontology Terms
#'
#' @description
#' Presents OLS search results for each query term in an interactive console
#' session, allowing the user to select, skip, or search for alternative terms.
#' Before searching, the function verifies that OLS is serving the package's
#' pinned Cell Ontology release (\code{2026-06-08}) and stops on a mismatch.
#'
#' @section Interactive commands:
#' \describe{
#'   \item{\code{1-N}}{Select candidate by number.}
#'   \item{\code{Enter}}{Accept the top-ranked candidate (rank 1).}
#'   \item{\code{0} or \code{n}}{Skip this query (no selection).}
#'   \item{\code{a}}{Accept defaults for all remaining matched queries
#'     (auto-accept top candidate without further review).}
#'   \item{\code{s}}{Skip the current query and continue reviewing.}
#'   \item{\code{p} / \code{c}}{Previous / next page of candidates.}
#'   \item{\code{r}}{Search for a new term dynamically.}
#'   \item{\code{i}}{Show detailed information for all candidates.}
#'   \item{\code{h} or \code{?}}{Show help.}
#'   \item{\code{q}}{Quit and return results collected so far.}
#' }
#'
#' @param query Character vector of cell type names or descriptions.
#' @param returnType Type of result: \code{"all"} (default), \code{"id"}, or
#'   \code{"label"}.
#' @param n_candidates Number of candidate matches to fetch per query
#'   (default: \code{20}).
#' @param auto_accept Logical; if \code{TRUE}, automatically accept the top
#'   match without user interaction (default: \code{FALSE}).  Automatically
#'   enabled in non-interactive sessions.
#' @param show_details Logical; if \code{TRUE}, show rank and normalised query
#'   for each candidate (default: \code{FALSE}).
#' @param items_per_page Number of candidates to display per page
#'   (default: \code{10}).
#'
#' @return Depends on \code{returnType}:
#'   \itemize{
#'     \item \code{"all"}: data frame with columns \code{query_original},
#'       \code{query_display}, \code{query_actual}, \code{cl_label},
#'       \code{cl_id}, \code{selection_mode}, \code{match_status},
#'       \code{user_selected}, and \code{ontology_release}.
#'     \item \code{"id"} / \code{"label"}: named character vector.
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Review candidate Cell Ontology terms in an interactive console session.
#' # Press Enter to accept the top candidate, type a number to select another
#' # candidate, or type "s" to skip the current query.
#' reviewed <- CLmapInteractive(
#'   c("T cell", "B cell", "classical monocyte"),
#'   n_candidates = 10
#' )
#' reviewed
#'
#' # Show more candidate details and fewer rows per page during review
#' detailed_review <- CLmapInteractive(
#'   c("helper T cell", "macrophage"),
#'   n_candidates = 20,
#'   show_details = TRUE,
#'   items_per_page = 5
#' )
#' detailed_review
#'
#' # Non-interactive workflows can auto-accept the top-ranked candidate.
#' # This is useful for scripts where manual review is not required.
#' auto_review <- CLmapInteractive(
#'   c("T cell", "B cell", "dendritic cell"),
#'   returnType = "id",
#'   n_candidates = 5,
#'   auto_accept = TRUE
#' )
#' auto_review
#'
#' # Return accepted labels instead of the full review table
#' accepted_labels <- CLmapInteractive(
#'   c("CD8 positive T cell", "plasma cell"),
#'   returnType = "label",
#'   auto_accept = TRUE
#' )
#' accepted_labels
#' }
CLmapInteractive <- function(query,
                             returnType     = c("all", "id", "label"),
                             n_candidates   = 20L,
                             auto_accept    = FALSE,
                             show_details   = FALSE,
                             items_per_page = 10L) {

  .display_candidates <- function(q_orig, q_display, q_actual, candidates,
                                  qi, n_total, show_details,
                                  cur_page, tot_pages, start_row, end_row, tot_rows) {
    cat("\n", strrep("\u2500", 72), "\n", sep = "")
    cat(sprintf("Query %d of %d\n", qi, n_total))
    cat(strrep("\u2500", 72), "\n", sep = "")
    cat("Original query:    ", q_orig,    "\n", sep = "")
    cat("Display query:     ", q_display, "\n", sep = "")
    cat("Normalised query:  ", q_actual,  "\n\n", sep = "")
    cat(sprintf("Candidates (page %d/%d, showing %d\u2013%d of %d):\n\n",
                cur_page, tot_pages, start_row, end_row, tot_rows))
    for (k in seq_len(end_row - start_row + 1L)) {
      row <- start_row + k - 1L
      cat(sprintf("  [%d] %s (%s)\n",
                  k, candidates$cl_label[row], candidates$cl_id[row]))
      if (show_details) {
        cat(sprintf("      Rank: %d | Normalised query: %s\n",
                    candidates$rank[row], candidates$query_actual[row]))
      }
    }
    cat("\n")
  }

  .display_no_candidates <- function(q_orig, q_display, q_actual,
                                     status, error_msg, qi, n_total) {
    cat("\n", strrep("\u2500", 72), "\n", sep = "")
    cat(sprintf("Query %d of %d\n", qi, n_total))
    cat(strrep("\u2500", 72), "\n", sep = "")
    cat("Original query:    ", q_orig,    "\n", sep = "")
    cat("Display query:     ", q_display, "\n", sep = "")
    cat("Normalised query:  ", q_actual,  "\n\n", sep = "")
    if (identical(status, "api_error")) {
      cat("Status: API error\nMessage: ", error_msg, "\n\n", sep = "")
    } else if (identical(status, "no_match")) {
      cat("Status: no match found in Cell Ontology.\n\n")
    } else {
      cat("Status: no result.\n\n")
    }
  }

  .show_help <- function() {
    cat("\n", strrep("\u2550", 72), "\n", sep = "")
    cat("Interactive Mapping Commands\n")
    cat(strrep("\u2550", 72), "\n\n", sep = "")
    cat("Selection:\n")
    cat("  1-N         Select candidate by number\n")
    cat("  Enter       Accept top candidate (rank 1)\n")
    cat("  0, n        Skip this query (no selection)\n")
    cat("  s           Skip this query and continue reviewing\n\n")
    cat("Batch:\n")
    cat("  a           Accept defaults for ALL remaining matched queries\n\n")
    cat("Navigation:\n")
    cat("  p           Previous page\n")
    cat("  c           Next page\n")
    cat("  r           Search for a new term\n\n")
    cat("Information:\n")
    cat("  i           Show detailed candidate info\n")
    cat("  h, ?        Show this help\n\n")
    cat("Control:\n")
    cat("  q           Quit and return results collected so far\n")
    cat(strrep("\u2550", 72), "\n\n", sep = "")
  }

  .show_info <- function(candidates) {
    cat("\n", strrep("\u2550", 72), "\n", sep = "")
    cat("Detailed Candidate Information\n")
    cat(strrep("\u2550", 72), "\n\n", sep = "")
    n_show <- min(nrow(candidates), 15L)
    for (i in seq_len(n_show)) {
      cat(sprintf("Rank %d:\n  Label:            %s\n  CL ID:            %s\n  Normalised query: %s\n\n",
                  candidates$rank[i], candidates$cl_label[i],
                  candidates$cl_id[i], candidates$query_actual[i]))
    }
    if (nrow(candidates) > 15L)
      cat(sprintf("... and %d more candidate(s)\n\n", nrow(candidates) - 15L))
    cat("Press Enter to continue...")
    readline()
  }

  # ========================================================================
  # Argument validation
  # ========================================================================

  returnType <- match.arg(returnType)

  if (missing(query) || is.null(query) || length(query) == 0L) {
    stop("`query` must be a non-empty character vector.", call. = FALSE)
  }
  prepared <- .map_prepare_queries(query)
  query_original_all <- prepared$original
  n_input <- length(query_original_all)

  n_candidates <- .validate_integer_scalar(
    n_candidates,
    "n_candidates",
    minimum = 1L
  )
  items_per_page <- .validate_integer_scalar(
    items_per_page,
    "items_per_page",
    minimum = 1L
  )
  auto_accept <- .validate_logical_scalar(auto_accept, "auto_accept")
  show_details <- .validate_logical_scalar(show_details, "show_details")

  valid_idx <- prepared$valid
  n_valid <- sum(valid_idx)

  if (n_valid > 0L && !auto_accept && !.map_is_interactive()) {
    warning("Non-interactive session detected. Enabling auto_accept.", call. = FALSE)
    auto_accept <- TRUE
  }
  if (n_valid > 0L && !requireNamespace("rols", quietly = TRUE)) {
    stop(
      "Package 'rols' is required. Install with: BiocManager::install('rols')",
      call. = FALSE
    )
  }

  # ========================================================================
  # Normalise and deduplicate
  # ========================================================================

  if (n_valid > 0L) .assert_ols_cl_release()

  query_norm_all <- prepared$normalized

  query_norm_unique <- unique(query_norm_all[valid_idx])
  n_unique <- length(query_norm_unique)

  rep_idx      <- match(query_norm_unique, query_norm_all)
  rep_original <- query_original_all[rep_idx]
  names(rep_original) <- query_norm_unique

  message("\n", strrep("=", 72))
  message("Interactive Cell Type Mapping")
  message(strrep("=", 72))
  message("Fetching candidates from Cell Ontology...")
  message("Total queries: ", n_input, " | Valid: ", n_valid,
          " | Unique to review: ", n_unique)
  if (n_unique < n_valid)
    message("  (", n_valid - n_unique, " duplicate(s) - one decision applied to all)")
  message("")

  # ========================================================================
  # Initial search cache
  # ========================================================================

  search_cache <- stats::setNames(vector("list", n_unique), query_norm_unique)
  for (i in seq_along(query_norm_unique)) {
    search_cache[[i]] <- .map_search_candidates(
      unname(rep_original[i]),
      n_candidates
    )
  }

  # ========================================================================
  # Interactive review loop
  # ========================================================================

  user_selections  <- stats::setNames(vector("list", n_unique), query_norm_unique)
  accept_remaining <- FALSE
  quit_early       <- FALSE

  for (qi in seq_along(query_norm_unique)) {
    q_norm    <- query_norm_unique[qi]
    q_orig    <- rep_original[[q_norm]]
    cur_info  <- search_cache[[q_norm]]
    cur_page  <- 1L

    repeat {
      has_cands <- nrow(cur_info$candidates) > 0L

      # Auto-accept path
      if (auto_accept || accept_remaining) {
        user_selections[[q_norm]] <- .map_default_selection(cur_info)
        break
      }

      # Display
      if (has_cands) {
        tot_rows  <- nrow(cur_info$candidates)
        tot_pages <- ceiling(tot_rows / items_per_page)
        cur_page  <- max(1L, min(cur_page, tot_pages))
        start_row <- (cur_page - 1L) * items_per_page + 1L
        end_row   <- min(cur_page * items_per_page, tot_rows)

        .display_candidates(q_orig, cur_info$query_display, cur_info$query_actual,
                            cur_info$candidates, qi, n_unique, show_details,
                            cur_page, tot_pages, start_row, end_row, tot_rows)
        sel <- .read_map_selection(
          end_row - start_row + 1L,
          cur_page,
          tot_pages
        )
      } else {
        .display_no_candidates(q_orig, cur_info$query_display, cur_info$query_actual,
                               cur_info$status, cur_info$error_message, qi, n_unique)
        sel <- .read_map_selection_no_candidates()
      }

      # Handle selection
      if (sel$action == "help")      { .show_help(); next }
      if (sel$action == "info")      { if (has_cands) .show_info(cur_info$candidates) else cat("No details available.\n"); next }
      if (sel$action == "next_page") { cur_page <- cur_page + 1L; next }
      if (sel$action == "prev_page") { cur_page <- cur_page - 1L; next }

      if (sel$action == "new_search") {
        message("\nSearching for: \"", sel$choice, "\"...")
        cur_info <- .map_search_candidates(sel$choice, n_candidates)
        cur_page <- 1L
        if (identical(cur_info$status, "matched"))
          message("Found ", nrow(cur_info$candidates), " match(es).\n")
        else if (identical(cur_info$status, "no_match"))
          message("No matches found.\n")
        else
          message("Search failed: ", cur_info$error_message, "\n")
        next
      }

      if (sel$action == "quit") {
        quit_early <- TRUE
        message("\n[Quit - returning results collected so far...]")
        break
      }

      if (sel$action == "accept_remaining") {
        accept_remaining <- TRUE
        user_selections[[q_norm]] <- .map_default_selection(cur_info)
        message("\n[Auto-accepting defaults for remaining matched queries...]")
        break
      }

      if (sel$action == "skip") {
        user_selections[[q_norm]] <- .map_make_selection(cur_info, "skip")
        break
      }

      if (sel$action %in% c("select", "accept_top")) {
        if (!has_cands) { cat("No candidates to select.\n"); next }
        abs_row <- if (identical(sel$action, "accept_top")) {
          1L
        } else {
          start_row - 1L + sel$choice
        }
        if (abs_row < 1L || abs_row > nrow(cur_info$candidates)) {
          cat("Selection out of bounds.\n"); next
        }
        user_selections[[q_norm]] <- .map_make_selection(
          cur_info,
          "manual",
          abs_row
        )
        break
      }

      stop("Unexpected action: ", sel$action)
    }

    if (quit_early) break
    if (qi < n_unique) message("")
  }

  # ========================================================================
  # Fill unresolved queries (quit_early or accept_remaining)
  # ========================================================================

  unresolved <- which(vapply(user_selections, function(x) length(x) == 0L, logical(1L)))
  for (idx in unresolved) {
    q_norm    <- query_norm_unique[idx]
    base_info <- search_cache[[q_norm]]

    user_selections[[q_norm]] <- .map_unresolved_selection(
      base_info,
      accept_remaining,
      quit_early
    )
  }

  # ========================================================================
  # Build final data frame
  # ========================================================================

  message("\n", strrep("-", 72))
  message("Building final results...")

  results_list <- lapply(seq_len(n_input), function(i) {
    q_orig   <- query_original_all[i]
    q_norm_i <- query_norm_all[i]

    if (!valid_idx[i]) {
      return(data.frame(
        query_original = q_orig, query_display = NA_character_,
        query_actual = NA_character_, cl_label = NA_character_,
        cl_id = NA_character_, selection_mode = "invalid_input",
        match_status = "invalid_input", user_selected = FALSE,
        ontology_release = .CL_RELEASE,
        stringsAsFactors = FALSE
      ))
    }

    sel <- user_selections[[q_norm_i]]
    data.frame(
      query_original = q_orig,
      query_display  = sel$query_display,
      query_actual   = sel$query_actual,
      cl_label       = sel$cl_label,
      cl_id          = sel$cl_id,
      selection_mode = sel$selection_mode,
      match_status   = sel$match_status,
      user_selected  = identical(sel$selection_mode, "manual"),
      ontology_release = .CL_RELEASE,
      stringsAsFactors = FALSE
    )
  })

  df <- do.call(rbind, results_list)
  rownames(df) <- NULL

  # ========================================================================
  # Summary
  # ========================================================================

  selection_modes <- vapply(
    user_selections,
    function(selection) selection$selection_mode,
    character(1L)
  )
  count_mode <- function(mode) sum(selection_modes == mode)
  n_selected <- sum(!is.na(df$cl_id))

  message("\n", strrep("=", 72))
  message("Mapping Session Summary")
  message(strrep("=", 72))
  message("Total input queries:           ", n_input)
  message("Valid queries:                 ", n_valid)
  message("Unique queries reviewed:       ", n_unique)
  message("")
  message("Selection modes (unique queries):")
  message("  Manual:                      ", count_mode("manual"))
  message("  Auto default:                ", count_mode("auto_default"))
  message("  Skipped by user:             ", count_mode("skip"))
  message("  No match:                    ", count_mode("no_match"))
  message("  API error:                   ", count_mode("api_error"))
  message("  Quit unreviewed:             ", count_mode("quit_unreviewed"))
  message("")
  message("Final mapped rows:             ", n_selected, " (",
          sprintf("%.1f%%", if (n_valid > 0L) 100 * n_selected / n_valid else 0),
          " of valid inputs)")
  message(strrep("=", 72), "\n")

  # ========================================================================
  # Return
  # ========================================================================

  if (returnType == "id") {
    out <- stats::setNames(df$cl_id, df$query_original)
    attr(out, "ontology_release") <- .CL_RELEASE
    return(out)
  }
  if (returnType == "label") {
    out <- stats::setNames(df$cl_label, df$query_original)
    attr(out, "ontology_release") <- .CL_RELEASE
    return(out)
  }
  df
}
