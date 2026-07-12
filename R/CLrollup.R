#' Greedily Roll Up Cell Ontology Terms to a Coarser Resolution
#'
#' @description
#' Aggregates a set of CL terms by replacing disjoint groups of related terms
#' with their most specific eligible shared ancestor, subject to a minimum
#' group-size constraint.
#'
#' @section Depth convention and specificity:
#' Throughout this package, **depth is synonymous with ancestor_count**:
#'
#' \deqn{ancestor\_count(id) = |\{ancestors\}| - 1}
#'
#' A term with a \emph{larger} ancestor_count is more specific (more granular).
#' Roll-up prefers the most specific valid ancestor, i.e. the candidate with the
#' largest ancestor_count that still covers at least \code{min_group_size} input
#' terms.
#'
#' The \code{max_ancestor_count} filter removes candidate ancestors whose
#' ancestor_count exceeds the threshold, preventing roll-up to overly specific
#' intermediate nodes.  The filter is applied before candidates are ranked, so
#' the most specific candidate that satisfies the threshold is selected.  A
#' value of 0 permits only root-level candidates.
#'
#' @section Grouping algorithm:
#' Candidate ancestors are ranked deterministically by decreasing
#' ancestor_count, then by increasing number of covered input terms, and
#' finally by CL ID.  Candidates are accepted greedily when they still cover at
#' least \code{min_group_size} unassigned terms.  Consequently, this function
#' prioritises local specificity rather than solving a global optimisation
#' problem, and applying it repeatedly to its own output may produce additional
#' roll-up.
#'
#' @param ids Character vector of CL IDs to aggregate.  \code{NA} and empty
#'   strings are removed, and duplicates are silently removed (set semantics).
#' @param clData An \code{ontology_index} object returned by \code{CLload()}.
#' @param min_group_size Finite positive integer giving the minimum number of
#'   distinct input terms that must share an ancestor for that ancestor to be
#'   used as a roll-up target (default: \code{2}).
#' @param max_ancestor_count \code{NULL} or a finite non-negative integer giving
#'   the upper bound on the ancestor_count of candidate ancestors (default:
#'   \code{NULL}, no limit).  Ancestors with
#'   \code{ancestor_count > max_ancestor_count} are excluded.
#' @param return_mapping A non-missing logical scalar; if \code{TRUE} (default),
#'   return a detailed list; if \code{FALSE}, return a named character vector.
#' @param verbose A non-missing logical scalar; if \code{TRUE} (default), print
#'   progress messages.
#'
#' @return
#' If \code{return_mapping = TRUE}, a list with:
#' \describe{
#'   \item{\code{mapping}}{Data frame with columns \code{original_id},
#'     \code{original_label}, \code{rolled_id}, \code{rolled_label},
#'     \code{was_rolled}.}
#'   \item{\code{groups}}{Named list mapping each ancestor actually selected by
#'     the greedy assignment to the input terms assigned to it.  Unselected
#'     candidates are omitted.  When \code{min_group_size = 1}, selected
#'     singleton groups are included even though their mapping is unchanged.}
#'   \item{\code{rolled_terms}}{Character vector of unique rolled-up CL IDs.}
#' }
#' If \code{return_mapping = FALSE}, a named character vector mapping each
#' original ID to its rolled-up ID.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clData <- CLload()
#'
#' ids <- c(
#'   "CL:0000624",  # CD4-positive, alpha-beta T cell
#'   "CL:0000625",  # CD8-positive, alpha-beta T cell
#'   "CL:0000236",  # B cell
#'   "CL:0000576",  # monocyte
#'   "CL:0000623",  # natural killer cell
#'   "CL:0000775",  # neutrophil
#'   "CL:0000771",  # eosinophil
#'   "CL:0000767"   # basophil
#' )
#'
#' result <- CLrollup(ids, clData, min_group_size = 2)
#' print(result$mapping[, c("original_label", "rolled_label", "was_rolled")])
#'
#' # Restrict to ancestors with ancestor_count <= 10
#' result2 <- CLrollup(ids, clData, min_group_size = 2, max_ancestor_count = 10)
#'
#' # Return only the named mapping vector
#' CLrollup(ids, clData, return_mapping = FALSE)
#' }
CLrollup <- function(ids,
                     clData,
                     min_group_size     = 2L,
                     max_ancestor_count = NULL,
                     return_mapping     = TRUE,
                     verbose            = TRUE) {

  # ---- Validate inputs ----
  .validate_cldata(clData)

  if (!requireNamespace("ontologyIndex", quietly = TRUE)) {
    stop("Package 'ontologyIndex' is required. Install with:\n",
         "  BiocManager::install('ontologyIndex')", call. = FALSE)
  }

  # Clean and deduplicate first, then enforce format and ontology membership
  # strictly.  The shared validator is intentionally permissive for several
  # query helpers, whereas roll-up cannot produce a meaningful mapping for a
  # malformed or unknown ID.
  ids <- .validate_ids(ids, clData = NULL, unique_only = TRUE,
                       allow_unknown = TRUE, warn_invalid = FALSE)

  bad_format <- unique(ids[!grepl("^CL:\\d+$", ids)])
  if (length(bad_format) > 0L) {
    stop(
      "Invalid CL ID format: ",
      paste(utils::head(bad_format, 3L), collapse = ", "),
      if (length(bad_format) > 3L) {
        paste0(" (and ", length(bad_format) - 3L, " more)")
      } else "",
      call. = FALSE
    )
  }

  unknown <- unique(ids[!ids %in% clData$id])
  if (length(unknown) > 0L) {
    stop(
      "Unknown CL IDs: ",
      paste(utils::head(unknown, 3L), collapse = ", "),
      if (length(unknown) > 3L) {
        paste0(" (and ", length(unknown) - 3L, " more)")
      } else "",
      call. = FALSE
    )
  }

  validate_integer_scalar <- function(x, name, minimum, null_ok = FALSE) {
    if (null_ok && is.null(x)) return(NULL)

    valid <- is.numeric(x) && length(x) == 1L && !is.na(x) &&
      is.finite(x) && x == floor(x) && x >= minimum &&
      x <= .Machine$integer.max

    if (!valid) {
      requirement <- if (minimum == 0L) {
        "NULL or a finite non-negative integer"
      } else {
        "a finite positive integer"
      }
      stop("`", name, "` must be ", requirement, ".", call. = FALSE)
    }

    as.integer(x)
  }

  validate_logical_scalar <- function(x, name) {
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
      stop("`", name, "` must be TRUE or FALSE.", call. = FALSE)
    }
    x
  }

  min_group_size <- validate_integer_scalar(
    min_group_size, "min_group_size", minimum = 1L
  )
  max_ancestor_count <- validate_integer_scalar(
    max_ancestor_count, "max_ancestor_count", minimum = 0L, null_ok = TRUE
  )
  return_mapping <- validate_logical_scalar(return_mapping, "return_mapping")
  verbose <- validate_logical_scalar(verbose, "verbose")

  if (min_group_size > length(ids)) {
    warning("`min_group_size` (", min_group_size,
            ") is larger than the number of input IDs (", length(ids),
            "). No roll-up will occur.", call. = FALSE)
  }

  if (verbose) {
    message("Rolling up ", length(ids), " terms...")
    message("  min_group_size:     ", min_group_size)
    if (!is.null(max_ancestor_count))
      message("  max_ancestor_count: ", max_ancestor_count)
  }

  # ---- Build ancestor -> covered-input-terms map ----
  all_ancestors <- lapply(ids, function(id) ontologyIndex::get_ancestors(clData, id))
  names(all_ancestors) <- ids

  unique_ancestors <- unique(unlist(all_ancestors, use.names = FALSE))

  ancestor_to_terms <- lapply(unique_ancestors, function(anc) {
    ids[vapply(all_ancestors, function(x) anc %in% x, logical(1L))]
  })
  names(ancestor_to_terms) <- unique_ancestors

  # ---- Filter by minimum group size ----
  ancestor_to_terms <- ancestor_to_terms[lengths(ancestor_to_terms) >= min_group_size]

  # ---- Apply max_ancestor_count filter ----
  # This must happen before redundant-parent removal.  Otherwise an ineligible
  # specific child can remove an eligible general parent and then itself be
  # discarded by the threshold, leaving no candidate for the group.
  if (!is.null(max_ancestor_count) && length(ancestor_to_terms) > 0L) {
    anc_counts <- .get_ancestor_count_vec(names(ancestor_to_terms), clData)
    before     <- length(ancestor_to_terms)
    ancestor_to_terms <- ancestor_to_terms[anc_counts <= max_ancestor_count]
    if (verbose && length(ancestor_to_terms) < before) {
      message("  Filtered out ", before - length(ancestor_to_terms),
              " ancestor(s) exceeding max_ancestor_count")
    }
  }

  # ---- Remove redundant (more general) ancestors ----
  # When a parent candidate covers exactly the same set of input terms as a
  # child candidate, the parent is redundant - the child already captures the
  # same grouping at finer resolution.  Drop the parent, keep the child.
  if (length(ancestor_to_terms) > 0L) {
    keep <- rep(TRUE, length(ancestor_to_terms))
    names(keep) <- names(ancestor_to_terms)

    for (anc in names(ancestor_to_terms)) {
      if (!keep[anc]) next
      covered <- ancestor_to_terms[[anc]]
      # Proper ancestors of `anc` that are also candidates
      anc_proper_ancestors <- setdiff(ontologyIndex::get_ancestors(clData, anc), anc)
      for (parent in intersect(anc_proper_ancestors, names(ancestor_to_terms))) {
        if (keep[parent] && setequal(ancestor_to_terms[[parent]], covered)) {
          keep[parent] <- FALSE   # parent is redundant - drop it
        }
      }
    }
    ancestor_to_terms <- ancestor_to_terms[keep]
  }

  # ---- Sort candidates: prefer most specific (largest ancestor_count),
  #      break ties by fewest covered terms, then by ID for stability ----
  if (length(ancestor_to_terms) > 0L) {
    anc_counts <- .get_ancestor_count_vec(names(ancestor_to_terms), clData)
    ord <- order(
      -anc_counts,                          # largest ancestor_count first
      lengths(ancestor_to_terms),           # fewest covered terms first
      names(ancestor_to_terms)              # alphabetical for stability
    )
    ancestor_to_terms <- ancestor_to_terms[ord]
  }

  # ---- Greedy assignment: most specific ancestor wins ----
  term_mapping   <- stats::setNames(ids, ids)   # identity mapping
  already_mapped <- character(0)
  selected_groups <- list()

  for (anc in names(ancestor_to_terms)) {
    unmapped <- setdiff(ancestor_to_terms[[anc]], already_mapped)
    if (length(unmapped) >= min_group_size) {
      term_mapping[unmapped] <- anc
      already_mapped <- c(already_mapped, unmapped)
      selected_groups[[anc]] <- unmapped
    }
  }

  # ---- Prepare output ----
  if (return_mapping) {
    mapping_df <- data.frame(
      original_id    = names(term_mapping),
      original_label = clData$name[names(term_mapping)],
      rolled_id      = unname(term_mapping),
      rolled_label   = clData$name[unname(term_mapping)],
      was_rolled     = names(term_mapping) != unname(term_mapping),
      stringsAsFactors = FALSE,
      row.names        = NULL
    )

    if (verbose) {
      n_rolled <- sum(mapping_df$was_rolled)
      n_final  <- length(unique(mapping_df$rolled_id))
      message("Roll-up completed:")
      message("  Input terms:  ", length(ids))
      message("  Rolled up:    ", n_rolled)
      message("  Final groups: ", n_final)
    }

    return(list(
      mapping      = mapping_df,
      groups       = selected_groups,
      rolled_terms = unique(unname(term_mapping))
    ))
  }

  if (verbose) {
    n_rolled <- sum(names(term_mapping) != unname(term_mapping))
    message("Roll-up completed: ", n_rolled, " term(s) rolled up.")
  }

  term_mapping
}
