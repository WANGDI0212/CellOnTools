# ============================================================================
# utils-map.R
# Shared query preparation and OLS candidate-search helpers.
# ============================================================================

.map_canonical_query <- function(query) {
  query <- gsub("[_/\\-]+", " ", query)
  query <- gsub("\\bcells\\b", "cell", query, ignore.case = TRUE)
  query <- gsub("\\s+", " ", query)
  trimws(query)
}

.map_normalize_query <- function(query) {
  tolower(.map_canonical_query(query))
}

.map_restore_cell_case <- function(query) {
  query <- gsub("\\bnkt\\b", "NKT", query, ignore.case = TRUE)
  query <- gsub("\\bnk\\b", "NK", query, ignore.case = TRUE)
  query <- gsub("\\bcd([0-9]+)\\b", "CD\\1", query, ignore.case = TRUE)
  query <- gsub("\\bt cell\\b", "T cell", query, ignore.case = TRUE)
  query <- gsub("\\bb cell\\b", "B cell", query, ignore.case = TRUE)
  query
}

.map_search_terms <- function(query_display) {
  canonical <- .map_canonical_query(query_display)
  terms <- unique(c(
    trimws(query_display),
    canonical,
    .map_restore_cell_case(canonical)
  ))
  terms[!is.na(terms) & nzchar(terms)]
}

.map_empty_candidates <- function() {
  data.frame(
    query_display = character(0),
    query_actual = character(0),
    cl_label = character(0),
    cl_id = character(0),
    rank = integer(0),
    stringsAsFactors = FALSE
  )
}

.map_ols_search <- function(term, rows) {
  search <- rols::OlsSearch(
    q = term,
    ontology = "cl",
    type = "class",
    groupField = TRUE,
    obsoletes = FALSE,
    rows = rows
  )
  as(rols::olsSearch(search), "data.frame")
}

.map_prepare_queries <- function(query) {
  original <- as.character(query)
  normalized <- rep(NA_character_, length(original))
  nonempty <- !is.na(original) & nzchar(trimws(original))

  if (any(nonempty)) {
    normalized[nonempty] <- .map_normalize_query(original[nonempty])
  }

  # A query made only of separators is non-empty before normalisation, but is
  # not searchable after canonicalisation.
  valid <- nonempty & !is.na(normalized) & nzchar(normalized)

  list(
    original = original,
    display = original,
    normalized = normalized,
    valid = valid
  )
}

.map_rerank_candidates <- function(candidates, query_actual) {
  if (nrow(candidates) <= 1L) return(candidates)

  label_lower <- tolower(candidates$cl_label)
  query_lower <- tolower(query_actual)
  score <- ifelse(label_lower == query_lower, 1L,
    ifelse(.map_normalize_query(label_lower) == query_lower, 2L,
      ifelse(startsWith(label_lower, query_lower), 3L,
        ifelse(grepl(query_lower, label_lower, fixed = TRUE), 4L, 5L)
      )
    )
  )

  candidates[order(score, seq_len(nrow(candidates))), , drop = FALSE]
}

.map_search_candidates <- function(query_display, max_results, verbose = FALSE) {
  query_actual <- if (!is.na(query_display)) {
    .map_normalize_query(query_display)
  } else {
    NA_character_
  }

  if (is.na(query_actual) || !nzchar(query_actual)) {
    return(list(
      query_display = query_display,
      query_actual = NA_character_,
      status = "invalid_input",
      error_message = NA_character_,
      candidates = .map_empty_candidates()
    ))
  }

  rows <- max(20L, max_results)
  errors <- character(0)
  results <- list()

  for (term in .map_search_terms(query_display)) {
    result <- tryCatch({
      result <- .map_ols_search(term, rows)
      required <- c("label", "obo_id")
      if (!all(required %in% colnames(result))) {
        stop("OLS result missing required columns: label, obo_id.")
      }

      result <- result[
        grep("^CL:\\d+$", result$obo_id),
        required,
        drop = FALSE
      ]
      colnames(result) <- c("cl_label", "cl_id")
      result
    }, error = function(error) {
      errors <<- c(errors, paste0(term, ": ", conditionMessage(error)))
      NULL
    })

    if (!is.null(result) && nrow(result) > 0L) {
      results[[length(results) + 1L]] <- result
    }
  }

  if (length(results) == 0L) {
    status <- if (length(errors) > 0L) "api_error" else "no_match"
    error_message <- if (length(errors) > 0L) {
      paste(unique(errors), collapse = " | ")
    } else {
      NA_character_
    }

    if (verbose && identical(status, "api_error")) {
      warning(
        "Search failed for '", query_actual, "': ", error_message,
        call. = FALSE
      )
    }

    return(list(
      query_display = query_display,
      query_actual = query_actual,
      status = status,
      error_message = error_message,
      candidates = .map_empty_candidates()
    ))
  }

  candidates <- do.call(rbind, results)
  candidates <- candidates[
    !duplicated(candidates[, c("cl_label", "cl_id")]),
    ,
    drop = FALSE
  ]
  candidates <- .map_rerank_candidates(candidates, query_actual)
  candidates <- candidates[
    seq_len(min(nrow(candidates), max_results)),
    ,
    drop = FALSE
  ]
  candidates <- data.frame(
    query_display = rep(query_display, nrow(candidates)),
    query_actual = rep(query_actual, nrow(candidates)),
    cl_label = candidates$cl_label,
    cl_id = candidates$cl_id,
    rank = seq_len(nrow(candidates)),
    stringsAsFactors = FALSE
  )

  list(
    query_display = query_display,
    query_actual = query_actual,
    status = "matched",
    error_message = NA_character_,
    candidates = candidates
  )
}

.map_make_selection <- function(info,
                                selection_mode = NULL,
                                candidate = NULL) {
  if (!is.null(candidate)) {
    return(list(
      query_display = info$query_display,
      query_actual = info$query_actual,
      cl_label = info$candidates$cl_label[candidate],
      cl_id = info$candidates$cl_id[candidate],
      selection_mode = selection_mode,
      match_status = "matched",
      error_message = NA_character_
    ))
  }

  list(
    query_display = info$query_display,
    query_actual = info$query_actual,
    cl_label = NA_character_,
    cl_id = NA_character_,
    selection_mode = if (is.null(selection_mode)) info$status else selection_mode,
    match_status = info$status,
    error_message = info$error_message
  )
}

.map_default_selection <- function(info) {
  if (identical(info$status, "matched") && nrow(info$candidates) > 0L) {
    .map_make_selection(info, "auto_default", 1L)
  } else {
    .map_make_selection(info)
  }
}

.map_unresolved_selection <- function(info, accept_remaining, quit_early) {
  if (accept_remaining && !quit_early) {
    return(.map_default_selection(info))
  }
  .map_make_selection(info, "quit_unreviewed")
}

.map_is_interactive <- function() {
  interactive()
}

.map_readline <- function(prompt) {
  readline(prompt = prompt)
}

.read_map_selection <- function(n_options, current_page, total_pages) {
  parts <- c(
    sprintf("Select [1\u2013%d]", n_options),
    "Enter=accept top",
    "0/n=skip this",
    "a=accept all remaining",
    "s=skip this"
  )
  if (total_pages > 1L) {
    if (current_page > 1L) parts <- c(parts, "p=prev page")
    if (current_page < total_pages) parts <- c(parts, "c=next page")
  }
  parts <- c(parts, "r=new search", "i=info", "q=quit", "h=help")
  prompt <- paste0(paste(parts, collapse = ", "), ": ")

  repeat {
    input <- trimws(tolower(.map_readline(prompt)))
    if (input == "") return(list(action = "accept_top", choice = NA_integer_))
    if (input %in% c("h", "?")) return(list(action = "help", choice = NA_integer_))
    if (input == "q") return(list(action = "quit", choice = NA_integer_))
    if (input == "a") return(list(action = "accept_remaining", choice = NA_integer_))
    if (input %in% c("s", "0", "n")) return(list(action = "skip", choice = NA_integer_))
    if (input == "i") return(list(action = "info", choice = NA_integer_))
    if (input == "p" && current_page > 1L) return(list(action = "prev_page", choice = NA_integer_))
    if (input == "c" && current_page < total_pages) return(list(action = "next_page", choice = NA_integer_))
    if (input %in% c("p", "c")) {
      cat("Already at boundary.\n")
      next
    }
    if (input == "r") {
      new_query <- trimws(.map_readline("Enter new search term: "))
      if (nzchar(.map_normalize_query(new_query))) {
        return(list(action = "new_search", choice = new_query))
      }
      cat("Search term cannot be empty.\n")
      next
    }

    if (!grepl("^[0-9]+$", input)) {
      cat("Invalid input. Type 'h' for help.\n")
      next
    }
    choice <- suppressWarnings(as.numeric(input))
    if (!is.finite(choice) || choice > .Machine$integer.max) {
      cat("Invalid input. Type 'h' for help.\n")
      next
    }
    choice <- as.integer(choice)
    if (choice < 1L || choice > n_options) {
      cat(sprintf("Enter a number between 1 and %d.\n", n_options))
      next
    }
    return(list(action = "select", choice = choice))
  }
}

.read_map_selection_no_candidates <- function() {
  prompt <- paste0(
    "0/n=continue, a=accept all remaining, r=new search, ",
    "q=quit, h=help: "
  )
  repeat {
    input <- trimws(tolower(.map_readline(prompt)))
    if (input %in% c("", "0", "n")) return(list(action = "skip", choice = NA_integer_))
    if (input %in% c("h", "?")) return(list(action = "help", choice = NA_integer_))
    if (input == "q") return(list(action = "quit", choice = NA_integer_))
    if (input == "a") return(list(action = "accept_remaining", choice = NA_integer_))
    if (input == "r") {
      new_query <- trimws(.map_readline("Enter new search term: "))
      if (nzchar(.map_normalize_query(new_query))) {
        return(list(action = "new_search", choice = new_query))
      }
      cat("Search term cannot be empty.\n")
      next
    }
    cat("Invalid input. Type 'h' for help.\n")
  }
}
