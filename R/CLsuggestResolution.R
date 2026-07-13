#' Suggest Optimal Resolution for Cell Ontology Term Roll-up
#'
#' @description
#' Exhaustively sweeps \code{min_group_size} from 1 through the number of
#' unique input IDs and, optionally,
#' \code{max_candidate_ancestor_count} values for \code{\link{CLrollup}}. It
#' recommends the parameter combination whose resulting number of groups is
#' closest to \code{target_groups}.
#'
#' @section Ancestor-count convention:
#' Candidate caps use the CL-only ancestor_count definition from
#' \code{\link{CLdepth}}. They are specificity limits, not graph-hop distances.
#'
#' @param ids Character vector of CL IDs.  Duplicates are silently removed.
#' @param clData An \code{ontology_index} object returned by \code{CLload()}.
#' @param target_groups Target number of final groups (default: \code{10}).
#' @param max_candidate_ancestor_count_values \code{NULL} or a vector of finite
#'   non-negative integer candidate ancestor-count caps to sweep in addition to
#'   \code{NULL} (no limit). Pass e.g. \code{c(5L, 10L, 20L)} to test several
#'   specificity caps.
#' @param verbose Logical; if \code{TRUE} (default), print a summary.
#'
#' @return Invisible list with:
#' \describe{
#'   \item{\code{best}}{Named list with the best parameter configuration.}
#'   \item{\code{all_candidates}}{List of all tested configurations.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clData <- CLload()
#'
#' ids <- c(
#'   "CL:0000624", "CL:0000625", "CL:0000236", "CL:0000576",
#'   "CL:0000623", "CL:0000775", "CL:0000771", "CL:0000767",
#'   "CL:0000451", "CL:0000097", "CL:0000235"
#' )
#'
#' suggestion <- CLsuggestResolution(ids, clData, target_groups = 3)
#'
#' # Apply recommended parameters
#' result <- CLrollup(ids, clData,
#'   min_group_size = suggestion$best$min_group_size,
#'   max_candidate_ancestor_count =
#'     suggestion$best$max_candidate_ancestor_count
#' )
#' }
CLsuggestResolution <- function(ids,
                                clData,
                                target_groups = 10L,
                                max_candidate_ancestor_count_values = NULL,
                                verbose = TRUE) {

  # ---- Validate inputs ----
  .validate_cldata(clData)

  if (!requireNamespace("ontologyIndex", quietly = TRUE)) {
    stop("Package 'ontologyIndex' is required. Install with:\n",
         "  BiocManager::install('ontologyIndex')", call. = FALSE)
  }

  ids <- .validate_ids(
    ids,
    clData = clData,
    unique_only = TRUE,
    allow_unknown = FALSE,
    warn_invalid = FALSE
  )
  target_groups <- .validate_integer_scalar(
    target_groups, "target_groups", minimum = 1L
  )
  verbose <- .validate_logical_scalar(verbose, "verbose")

  caps <- max_candidate_ancestor_count_values
  if (is.null(caps)) {
    caps <- integer(0)
  } else {
    valid_caps <- is.numeric(caps) && length(caps) > 0L &&
      all(!is.na(caps)) && all(is.finite(caps)) &&
      all(caps == floor(caps)) && all(caps >= 0) &&
      all(caps <= .Machine$integer.max)
    if (!valid_caps) {
      stop(
        "`max_candidate_ancestor_count_values` must be NULL or a vector ",
        "of finite non-negative integers.",
        call. = FALSE
      )
    }
    caps <- unique(as.integer(caps))
  }

  if (target_groups > length(ids)) {
    warning("`target_groups` (", target_groups,
            ") is larger than the number of input IDs (", length(ids), ").",
            call. = FALSE)
  }

  # ---- Build sweep grid ----
  # Every value is required for correctness. Roll-up group counts can be
  # non-monotonic on overlapping DAG candidates, so a target-derived upper
  # bound can skip the closest (or exact) solution. min_group_size = 1 is also
  # the identity-resolution configuration and is needed when target_groups is
  # equal or close to the number of input terms.
  max_min_size <- length(ids)
  min_sizes <- seq_len(max_min_size)

  # Always include NULL (no specificity cap).
  mac_values <- c(list(NULL), as.list(caps))

  if (verbose) {
    message("Sweeping min_group_size from 1 to ", max_min_size,
            " across ", length(mac_values),
            " candidate ancestor-count value(s)...")
  }

  results <- vector("list", length(min_sizes) * length(mac_values))
  idx <- 0L

  for (mac in mac_values) {
    for (min_size in min_sizes) {
      idx <- idx + 1L

      rollup_error <- NULL
      rollup_result <- tryCatch(
        CLrollup(
          ids = ids,
          clData = clData,
          min_group_size = min_size,
          max_candidate_ancestor_count = mac,
          return_mapping = TRUE,
          verbose = FALSE
        ),
        error = function(e) {
          rollup_error <<- conditionMessage(e)
          NULL
        }
      )

      if (is.null(rollup_result)) {
        results[[idx]] <- list(
          min_group_size = min_size,
          max_candidate_ancestor_count = mac,
          n_groups = NA_integer_,
          n_rolled = NA_integer_,
          compression_ratio = NA_real_,
          deviation = NA_integer_,
          error = rollup_error
        )
        next
      }

      n_groups <- length(rollup_result$rolled_terms)
      n_rolled <- sum(rollup_result$mapping$was_rolled)

      results[[idx]] <- list(
        min_group_size = min_size,
        max_candidate_ancestor_count = mac,
        n_groups = n_groups,
        n_rolled = n_rolled,
        compression_ratio = round(n_groups / length(ids), 3),
        deviation = abs(n_groups - target_groups),
        error = NULL
      )
    }
  }

  # ---- Find best configuration ----
  deviations <- vapply(results, function(r) {
    if (is.null(r) || is.na(r$deviation)) Inf else r$deviation
  }, numeric(1L))

  if (all(!is.finite(deviations))) {
    errors <- unique(Filter(nzchar, vapply(
      results,
      function(r) if (is.null(r$error)) "" else r$error,
      character(1L)
    )))
    stop(
      "All roll-up configurations failed",
      if (length(errors) > 0L) paste0(": ", paste(errors, collapse = "; ")) else ".",
      call. = FALSE
    )
  }

  best_idx <- which.min(deviations)
  best     <- results[[best_idx]]

  if (verbose) {
    mac_str <- if (is.null(best$max_candidate_ancestor_count)) {
      "none"
    } else {
      best$max_candidate_ancestor_count
    }
    message("\n=== Optimal Resolution for ", target_groups, " target group(s) ===")
    message("  Recommended min_group_size:     ", best$min_group_size)
    message("  Recommended max_candidate_ancestor_count: ", mac_str)
    message("  Resulting groups:               ", best$n_groups)
    message("  Terms rolled up:                ", best$n_rolled, " / ", length(ids))
    message("  Compression ratio:              ", best$compression_ratio)

    if (length(results) > 1L) {
      message("\nAlternative configurations:")
      for (i in seq_along(results)) {
        if (i == best_idx) next
        r <- results[[i]]
        if (is.null(r) || is.na(r$n_groups)) next
        mac_s <- if (is.null(r$max_candidate_ancestor_count)) {
          "none"
        } else {
          r$max_candidate_ancestor_count
        }
        message(sprintf(
          paste0("  min_group_size=%d, ",
                 "max_candidate_ancestor_count=%s: %d groups, deviation=%d"),
          r$min_group_size, mac_s, r$n_groups, r$deviation
        ))
      }
    }
  }

  invisible(list(best = best, all_candidates = results))
}
