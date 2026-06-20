#' Interactively Map Cell Type Names to Cell Ontology Terms
#'
#' @description
#' Presents OLS search results for each query term in an interactive console
#' session, allowing the user to select, skip, or search for alternative terms.
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
#'       \code{user_selected}.
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

  # ========================================================================
  # Private helpers
  # ========================================================================

  .canonical_query <- function(q) {
    q <- gsub("[_/\\-]+", " ", q)
    q <- gsub("\\bcells\\b", "cell", q, ignore.case = TRUE)
    q <- gsub("\\s+", " ", q)
    trimws(q)
  }

  .normalize_query <- function(q) {
    tolower(.canonical_query(q))
  }

  .restore_common_cell_case <- function(q) {
    q <- gsub("\\bnkt\\b", "NKT", q, ignore.case = TRUE)
    q <- gsub("\\bnk\\b",  "NK",  q, ignore.case = TRUE)
    q <- gsub("\\bcd([0-9]+)\\b", "CD\\1", q, ignore.case = TRUE)
    q <- gsub("\\bt cell\\b", "T cell", q, ignore.case = TRUE)
    q <- gsub("\\bb cell\\b", "B cell", q, ignore.case = TRUE)
    q
  }

  .search_terms <- function(q_display) {
    unique(c(
      trimws(q_display),
      .canonical_query(q_display),
      .restore_common_cell_case(.canonical_query(q_display))
    ))
  }

  # Local reranking (same logic as CLmap)
  .rerank <- function(df_cl, query_actual) {
    if (nrow(df_cl) <= 1L) return(df_cl)
    lbl_lower <- tolower(df_cl$cl_label)
    q_lower   <- tolower(query_actual)
    score <- ifelse(lbl_lower == q_lower,                          1L,
             ifelse(.normalize_query(lbl_lower) == q_lower,        2L,
             ifelse(startsWith(lbl_lower, q_lower),                3L,
             ifelse(grepl(q_lower, lbl_lower, fixed = TRUE),       4L,
                                                                   5L))))
    df_cl[order(score, seq_len(nrow(df_cl))), , drop = FALSE]
  }

  .empty_candidates <- function() {
    data.frame(query_display = character(0), query_actual = character(0),
               cl_label = character(0), cl_id = character(0),
               rank = integer(0), stringsAsFactors = FALSE)
  }

  .search_candidates <- function(query_display, max_results) {
    if (is.na(query_display) || !nzchar(trimws(query_display))) {
      return(list(query_display = query_display, query_actual = NA_character_,
                  status = "invalid_query", error_message = NA_character_,
                  candidates = .empty_candidates()))
    }
    query_actual <- .normalize_query(query_display)
    rows <- max(20L, max_results)
    terms <- .search_terms(query_display)
    errors <- character(0)
    dfs <- list()

    for (term in terms) {
      df <- tryCatch({
        obj <- rols::OlsSearch(q = term, ontology = "cl", type = "class",
                               groupField = TRUE, obsoletes = FALSE,
                               rows = rows)
        df  <- as(rols::olsSearch(obj), "data.frame")

        if (!all(c("label", "obo_id") %in% colnames(df))) {
          stop("OLS result missing required columns.")
        }

        df[grep("^CL:", df$obo_id), c("label", "obo_id"), drop = FALSE]
      }, error = function(e) {
        errors <<- c(errors, paste0(term, ": ", conditionMessage(e)))
        NULL
      })

      if (!is.null(df) && nrow(df) > 0L) {
        dfs[[length(dfs) + 1L]] <- df
      }
    }

    if (length(dfs) == 0L) {
      if (length(errors) > 0L) {
        return(list(query_display = query_display, query_actual = query_actual,
                    status = "api_error",
                    error_message = paste(unique(errors), collapse = " | "),
                    candidates = .empty_candidates()))
      }
      return(list(query_display = query_display, query_actual = query_actual,
                  status = "no_match", error_message = NA_character_,
                  candidates = .empty_candidates()))
    }

    df_cl <- do.call(rbind, dfs)
    df_cl <- df_cl[!duplicated(df_cl[, c("label", "obo_id")]), , drop = FALSE]
    colnames(df_cl) <- c("cl_label", "cl_id")

    if (nrow(df_cl) == 0L) {
      return(list(query_display = query_display, query_actual = query_actual,
                  status = "no_match", error_message = NA_character_,
                  candidates = .empty_candidates()))
    }

    df_cl <- .rerank(df_cl, query_actual)
    n_take <- min(nrow(df_cl), max_results)
    df_cl  <- df_cl[seq_len(n_take), , drop = FALSE]

    candidates <- data.frame(
      query_display = rep(query_display, nrow(df_cl)),
      query_actual  = rep(query_actual,  nrow(df_cl)),
      cl_label      = df_cl$cl_label,
      cl_id         = df_cl$cl_id,
      rank          = seq_len(nrow(df_cl)),
      stringsAsFactors = FALSE
    )

    list(query_display = query_display, query_actual = query_actual,
         status = "matched", error_message = NA_character_,
         candidates = candidates)
  }

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

  .get_selection <- function(n_opts, cur_page, tot_pages) {
    parts <- c(sprintf("Select [1\u2013%d]", n_opts), "Enter=accept top",
               "0/n=skip this", "a=accept all remaining", "s=skip this")
    if (tot_pages > 1L) {
      if (cur_page > 1L)      parts <- c(parts, "p=prev page")
      if (cur_page < tot_pages) parts <- c(parts, "c=next page")
    }
    parts <- c(parts, "r=new search", "i=info", "q=quit", "h=help")
    prompt <- paste0(paste(parts, collapse = ", "), ": ")

    repeat {
      inp <- trimws(tolower(readline(prompt = prompt)))
      if (inp == "")                          return(list(action = "select",           choice = 1L))
      if (inp %in% c("h", "?"))              return(list(action = "help",             choice = NA_integer_))
      if (inp == "q")                         return(list(action = "quit",             choice = NA_integer_))
      if (inp == "a")                         return(list(action = "accept_remaining", choice = NA_integer_))
      if (inp == "s")                         return(list(action = "skip",             choice = NA_integer_))
      if (inp %in% c("0", "n"))              return(list(action = "skip",             choice = NA_integer_))
      if (inp == "i")                         return(list(action = "info",             choice = NA_integer_))
      if (inp == "p" && cur_page > 1L)       return(list(action = "prev_page",        choice = NA_integer_))
      if (inp == "c" && cur_page < tot_pages) return(list(action = "next_page",        choice = NA_integer_))
      if (inp == "p" || inp == "c") { cat("Already at boundary.\n"); next }
      if (inp == "r") {
        nq <- trimws(readline(prompt = "Enter new search term: "))
        if (nzchar(nq)) return(list(action = "new_search", choice = nq))
        cat("Search term cannot be empty.\n"); next
      }
      ch <- suppressWarnings(as.integer(inp))
      if (is.na(ch)) { cat("Invalid input. Type 'h' for help.\n"); next }
      if (ch < 1L || ch > n_opts) { cat(sprintf("Enter a number between 1 and %d.\n", n_opts)); next }
      return(list(action = "select", choice = ch))
    }
  }

  .get_selection_no_candidates <- function() {
    prompt <- "0/n=continue, a=accept all remaining, r=new search, q=quit, h=help: "
    repeat {
      inp <- trimws(tolower(readline(prompt = prompt)))
      if (inp %in% c("", "0", "n")) return(list(action = "skip",             choice = NA_integer_))
      if (inp %in% c("h", "?"))     return(list(action = "help",             choice = NA_integer_))
      if (inp == "q")                return(list(action = "quit",             choice = NA_integer_))
      if (inp == "a")                return(list(action = "accept_remaining", choice = NA_integer_))
      if (inp == "r") {
        nq <- trimws(readline(prompt = "Enter new search term: "))
        if (nzchar(nq)) return(list(action = "new_search", choice = nq))
        cat("Search term cannot be empty.\n"); next
      }
      cat("Invalid input. Type 'h' for help.\n")
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

  .make_record <- function(q_display, q_actual, cl_label = NA_character_,
                           cl_id = NA_character_, selection_mode, match_status,
                           error_message = NA_character_) {
    list(query_display = q_display, query_actual = q_actual,
         cl_label = cl_label, cl_id = cl_id,
         selection_mode = selection_mode, match_status = match_status,
         error_message = error_message)
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

  if (!is.numeric(n_candidates) || length(n_candidates) != 1L ||
      is.na(n_candidates) || n_candidates < 1L) {
    stop("`n_candidates` must be a positive integer.", call. = FALSE)
  }
  n_candidates <- as.integer(n_candidates)

  if (!is.numeric(items_per_page) || length(items_per_page) != 1L ||
      is.na(items_per_page) || items_per_page < 1L) {
    stop("`items_per_page` must be a positive integer.", call. = FALSE)
  }
  items_per_page <- as.integer(items_per_page)

  if (!is.logical(auto_accept) || is.na(auto_accept)) {
    stop("`auto_accept` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!auto_accept && !interactive()) {
    warning("Non-interactive session detected. Enabling auto_accept.", call. = FALSE)
    auto_accept <- TRUE
  }
  if (!requireNamespace("rols", quietly = TRUE)) {
    stop("Package 'rols' is required. Install with: BiocManager::install('rols')",
         call. = FALSE)
  }

  # ========================================================================
  # Normalise and deduplicate
  # ========================================================================

  valid_idx <- !is.na(query_original_all) & nzchar(trimws(query_original_all))
  n_valid   <- sum(valid_idx)
  if (n_valid == 0L) stop("No valid queries to process.", call. = FALSE)

  query_norm_all <- rep(NA_character_, n_input)
  query_norm_all[valid_idx] <- .normalize_query(query_original_all[valid_idx])

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
  for (q_norm in query_norm_unique) {
    search_cache[[q_norm]] <- .search_candidates(rep_original[[q_norm]], n_candidates)
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
        user_selections[[q_norm]] <- if (has_cands) {
          .make_record(cur_info$query_display, cur_info$query_actual,
                       cur_info$candidates$cl_label[1L],
                       cur_info$candidates$cl_id[1L],
                       "auto_default", "matched")
        } else {
          .make_record(cur_info$query_display, cur_info$query_actual,
                       selection_mode = cur_info$status,
                       match_status   = cur_info$status,
                       error_message  = cur_info$error_message)
        }
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
        sel <- .get_selection(end_row - start_row + 1L, cur_page, tot_pages)
      } else {
        .display_no_candidates(q_orig, cur_info$query_display, cur_info$query_actual,
                               cur_info$status, cur_info$error_message, qi, n_unique)
        sel <- .get_selection_no_candidates()
      }

      # Handle selection
      if (sel$action == "help")      { .show_help(); next }
      if (sel$action == "info")      { if (has_cands) .show_info(cur_info$candidates) else cat("No details available.\n"); next }
      if (sel$action == "next_page") { cur_page <- cur_page + 1L; next }
      if (sel$action == "prev_page") { cur_page <- cur_page - 1L; next }

      if (sel$action == "new_search") {
        message("\nSearching for: \"", sel$choice, "\"...")
        cur_info <- .search_candidates(sel$choice, n_candidates)
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
        # Record current query with top candidate (or no-match)
        user_selections[[q_norm]] <- if (has_cands) {
          .make_record(cur_info$query_display, cur_info$query_actual,
                       cur_info$candidates$cl_label[1L],
                       cur_info$candidates$cl_id[1L],
                       "auto_default", "matched")
        } else {
          .make_record(cur_info$query_display, cur_info$query_actual,
                       selection_mode = cur_info$status,
                       match_status   = cur_info$status,
                       error_message  = cur_info$error_message)
        }
        message("\n[Auto-accepting defaults for remaining matched queries...]")
        break
      }

      if (sel$action == "skip") {
        user_selections[[q_norm]] <- .make_record(
          cur_info$query_display, cur_info$query_actual,
          selection_mode = "skip",
          match_status   = if (has_cands) "matched" else cur_info$status,
          error_message  = cur_info$error_message
        )
        break
      }

      if (sel$action == "select") {
        if (!has_cands) { cat("No candidates to select.\n"); next }
        abs_row <- start_row - 1L + sel$choice
        if (abs_row < 1L || abs_row > nrow(cur_info$candidates)) {
          cat("Selection out of bounds.\n"); next
        }
        user_selections[[q_norm]] <- .make_record(
          cur_info$query_display, cur_info$query_actual,
          cur_info$candidates$cl_label[abs_row],
          cur_info$candidates$cl_id[abs_row],
          "manual", "matched"
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

    if (accept_remaining && !quit_early &&
        identical(base_info$status, "matched") &&
        nrow(base_info$candidates) > 0L) {
      user_selections[[q_norm]] <- .make_record(
        base_info$query_display, base_info$query_actual,
        base_info$candidates$cl_label[1L], base_info$candidates$cl_id[1L],
        "auto_default", "matched"
      )
    } else {
      user_selections[[q_norm]] <- .make_record(
        base_info$query_display, base_info$query_actual,
        selection_mode = "quit_unreviewed",
        match_status   = base_info$status,
        error_message  = base_info$error_message
      )
    }
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
      stringsAsFactors = FALSE
    )
  })

  df <- do.call(rbind, results_list)
  rownames(df) <- NULL

  # ========================================================================
  # Summary
  # ========================================================================

  u_sel <- do.call(rbind, lapply(user_selections, function(x)
    data.frame(selection_mode = x$selection_mode, stringsAsFactors = FALSE)))
  count_mode <- function(m) sum(u_sel$selection_mode == m)
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

  if (returnType == "id")    return(stats::setNames(df$cl_id,    df$query_original))
  if (returnType == "label") return(stats::setNames(df$cl_label, df$query_original))
  df
}
