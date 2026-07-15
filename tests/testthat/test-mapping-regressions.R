.mock_map_candidates <- function(query_display, status = "matched", n = 12L) {
  query_actual <- CellOnTools:::.map_normalize_query(query_display)
  candidates <- if (identical(status, "matched")) {
    data.frame(
      query_display = rep(query_display, n),
      query_actual = rep(query_actual, n),
      cl_label = paste("candidate", seq_len(n)),
      cl_id = sprintf("CL:%07d", seq_len(n)),
      rank = seq_len(n),
      stringsAsFactors = FALSE
    )
  } else {
    CellOnTools:::.map_empty_candidates()
  }

  list(
    query_display = query_display,
    query_actual = query_actual,
    status = status,
    error_message = if (identical(status, "api_error")) {
      "mock service failure"
    } else {
      NA_character_
    },
    candidates = candidates
  )
}

test_that("mapping query normalisation is shared and rejects separator-only input", {
  expect_equal(
    CellOnTools:::.map_normalize_query(" CD8_T-cells "),
    "cd8 t cell"
  )

  mapped <- CLmap(c("---", " /_ ", NA_character_), verbose = FALSE)
  expect_equal(mapped$match_status, rep("invalid_input", 3L))
  expect_true(all(is.na(mapped$query_actual)))

  capture.output(
    reviewed <- suppressMessages(CLmapInteractive(
      c("---", " /_ ", NA_character_),
      auto_accept = TRUE
    ))
  )
  expect_equal(reviewed$match_status, rep("invalid_input", 3L))
  expect_equal(reviewed$selection_mode, rep("invalid_input", 3L))
})

test_that("mapping count arguments require finite whole numbers", {
  invalid_counts <- list(1.5, Inf, NaN, NA_real_, c(1, 2))

  for (value in invalid_counts) {
    expect_error(
      CLmap("T cell", max_results = value, verbose = FALSE),
      "finite positive integer"
    )
    expect_error(
      CLmapInteractive("T cell", n_candidates = value, auto_accept = TRUE),
      "finite positive integer"
    )
    expect_error(
      CLmapInteractive("T cell", items_per_page = value, auto_accept = TRUE),
      "finite positive integer"
    )
  }
})

test_that("CLmap searches each shared normalised query once", {
  skip_if_not_installed("rols")
  search_count <- 0L

  testthat::local_mocked_bindings(
    .assert_ols_cl_release = function() "2026-06-08",
    .map_search_candidates = function(query_display, max_results, verbose = FALSE) {
      search_count <<- search_count + 1L
      .mock_map_candidates(query_display, n = 1L)
    },
    .package = "CellOnTools"
  )

  result <- CLmap(
    c("T_cells", "t-cell", "---"),
    verbose = FALSE
  )

  expect_equal(search_count, 1L)
  expect_equal(result$match_status, c("matched", "matched", "invalid_input"))
  expect_equal(result$cl_id[1:2], rep("CL:0000001", 2L))
  expect_true(is.na(result$cl_id[3L]))
})

test_that("Enter accepts global rank 1 after navigating to page 2", {
  skip_if_not_installed("rols")

  commands <- list(
    list(action = "next_page", choice = NA_integer_),
    list(action = "accept_top", choice = NA_integer_)
  )
  command_index <- 0L

  testthat::local_mocked_bindings(
    .assert_ols_cl_release = function() "2026-06-08",
    .map_is_interactive = function() TRUE,
    .map_search_candidates = function(query_display, max_results, verbose = FALSE) {
      .mock_map_candidates(query_display, n = 12L)
    },
    .read_map_selection = function(n_options, current_page, total_pages) {
      command_index <<- command_index + 1L
      commands[[command_index]]
    },
    .package = "CellOnTools"
  )

  capture.output(
    result <- suppressMessages(CLmapInteractive(
      "test query",
      n_candidates = 12L,
      items_per_page = 5L
    ))
  )

  expect_equal(command_index, 2L)
  expect_equal(result$cl_id, "CL:0000001")
  expect_equal(result$selection_mode, "manual")
})

test_that("accept remaining preserves no-match and API-error states", {
  skip_if_not_installed("rols")

  testthat::local_mocked_bindings(
    .assert_ols_cl_release = function() "2026-06-08",
    .map_is_interactive = function() TRUE,
    .map_search_candidates = function(query_display, max_results, verbose = FALSE) {
      status <- switch(
        query_display,
        matched = "matched",
        absent = "no_match",
        broken = "api_error"
      )
      .mock_map_candidates(query_display, status = status, n = 2L)
    },
    .read_map_selection = function(n_options, current_page, total_pages) {
      list(action = "accept_remaining", choice = NA_integer_)
    },
    .package = "CellOnTools"
  )

  capture.output(
    result <- suppressMessages(CLmapInteractive(
      c("matched", "absent", "broken"),
      n_candidates = 2L,
      items_per_page = 2L
    ))
  )

  expect_equal(
    result$selection_mode,
    c("auto_default", "no_match", "api_error")
  )
  expect_equal(result$match_status, c("matched", "no_match", "api_error"))
})

test_that("OLS candidate search filters, deduplicates, and reranks results", {
  searched_terms <- character()
  testthat::local_mocked_bindings(
    .map_ols_search = function(term, rows) {
      searched_terms <<- c(searched_terms, term)
      if (length(searched_terms) == 1L) {
        data.frame(
          label = c("unrelated", "T cell", "not a CL term"),
          obo_id = c("CL:0000001", "CL:0000002", "UBERON:0000001"),
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(
          label = c("T cell", "T lymphocyte"),
          obo_id = c("CL:0000002", "CL:0000003"),
          stringsAsFactors = FALSE
        )
      }
    },
    .package = "CellOnTools"
  )

  result <- CellOnTools:::.map_search_candidates("T_cells", 3L)
  expect_equal(searched_terms, c("T_cells", "T cell"))
  expect_identical(result$status, "matched")
  expect_equal(result$candidates$cl_id,
               c("CL:0000002", "CL:0000001", "CL:0000003"))
  expect_equal(result$candidates$rank, 1:3)
})

test_that("OLS candidate search distinguishes no matches from API errors", {
  testthat::local_mocked_bindings(
    .map_ols_search = function(term, rows) {
      data.frame(label = character(), obo_id = character())
    },
    .package = "CellOnTools"
  )
  no_match <- CellOnTools:::.map_search_candidates("absent", 3L)
  expect_identical(no_match$status, "no_match")

  testthat::local_mocked_bindings(
    .map_ols_search = function(term, rows) stop("service unavailable"),
    .package = "CellOnTools"
  )
  api_error <- suppressWarnings(
    CellOnTools:::.map_search_candidates("broken", 3L)
  )
  expect_identical(api_error$status, "api_error")
  expect_match(api_error$error_message, "service unavailable")
})

test_that("interactive numeric choices require whole-number text", {
  inputs <- c("1.5", "2")
  testthat::local_mocked_bindings(
    .map_readline = function(prompt) {
      value <- inputs[[1L]]
      inputs <<- inputs[-1L]
      value
    },
    .package = "CellOnTools"
  )

  capture.output(
    selection <- CellOnTools:::.read_map_selection(3L, 1L, 1L)
  )
  expect_identical(selection$action, "select")
  expect_identical(selection$choice, 2L)
})
