# ============================================================================
# utils-ontology.R
# Internal ontology computation helpers for the CellOnTools package.
# All functions are unexported (prefixed with a dot).
#
# PACKAGE-WIDE ANCESTOR-COUNT CONVENTION
# =======================================
# "ancestor_count" in this package counts proper ancestors in the CL
# namespace only:
#
#   ancestor_count(id) = | { a in ancestors(id) : a starts with "CL:" } | - 1L
#
# The subtraction removes the queried CL term itself. Imported BFO, CARO, and
# other non-CL ontology nodes are deliberately excluded. This is NOT graph-hop
# distance, shortest-path depth, or longest-path depth.
#
# Consequences:
#   - The root term has ancestor_count = 0.
#   - A direct child of the root has ancestor_count = 1.
#   - In a DAG, a term may have multiple CL ancestry branches; ancestor_count
#     counts unique CL ancestors, not path length.
# ============================================================================

# Return all well-formed CL identifiers present in an ontology object.
.get_cl_ids <- function(clData) {
  clData$id[grepl("^CL:\\d+$", clData$id)]
}

# ----------------------------------------------------------------------------
# .get_ancestor_count_one
# Return the ancestor_count for a single CL ID.
# Assumes the ID is already validated and present in clData.
# ----------------------------------------------------------------------------
.get_ancestor_count_one <- function(id, clData) {
  ancestors <- ontologyIndex::get_ancestors(clData, id)
  as.integer(sum(grepl("^CL:\\d+$", ancestors)) - 1L)
}

# ----------------------------------------------------------------------------
# .get_ancestor_count_vec
# Return a named integer vector of ancestor_counts for a vector of CL IDs.
# Assumes all IDs are already validated and present in clData.
# ----------------------------------------------------------------------------
.get_ancestor_count_vec <- function(ids, clData) {
  stats::setNames(
    vapply(ids, .get_ancestor_count_one, integer(1L), clData = clData),
    ids
  )
}

# ----------------------------------------------------------------------------
# .walk_ontology
# Breadth-first traversal over a named parent/child adjacency list. The result
# is ordered by hop distance, is cycle-safe, and is restricted to allowed_ids.
# ----------------------------------------------------------------------------
.walk_ontology <- function(start,
                           neighbours,
                           max_hops = NULL,
                           include_self = FALSE,
                           allowed_ids = NULL) {
  max_hops <- .validate_integer_scalar(
    max_hops, "max_hops", minimum = 0L, null_ok = TRUE
  )
  include_self <- .validate_logical_scalar(include_self, "include_self")

  if (is.null(allowed_ids)) {
    allowed_ids <- names(neighbours)
  }

  seen <- start
  frontier <- start
  result <- if (include_self) start else character(0)
  hop <- 0L

  while (length(frontier) > 0L &&
         (is.null(max_hops) || hop < max_hops)) {
    next_nodes <- unique(as.character(unlist(
      neighbours[frontier],
      use.names = FALSE
    )))
    next_nodes <- intersect(next_nodes, allowed_ids)
    next_nodes <- setdiff(next_nodes, seen)

    if (length(next_nodes) == 0L) break

    result <- c(result, next_nodes)
    seen <- c(seen, next_nodes)
    frontier <- next_nodes
    hop <- hop + 1L
  }

  result
}
