# ============================================================================
# utils-ontology.R
# Internal ontology computation helpers for the CellOnTools package.
# All functions are unexported (prefixed with a dot).
#
# PACKAGE-WIDE DEPTH CONVENTION
# ==============================
# "depth" in this package is defined as **ancestor_count**:
#
#   ancestor_count(id) = length(ontologyIndex::get_ancestors(clData, id)) - 1L
#
# That is, the number of ancestors of a term *excluding the term itself*.
# This is NOT the number of edges from the root, NOT the shortest path, and
# NOT the longest path.  It is simply the cardinality of the proper ancestor
# set as returned by ontologyIndex::get_ancestors().
#
# Consequences:
#   - The root term has ancestor_count = 0.
#   - A direct child of the root has ancestor_count = 1.
#   - In a DAG, a term may have multiple paths to the root; ancestor_count
#     counts unique ancestors, not path length.
# ============================================================================

# ----------------------------------------------------------------------------
# .get_ancestor_count_one
# Return the ancestor_count for a single CL ID.
# Assumes the ID is already validated and present in clData.
# ----------------------------------------------------------------------------
.get_ancestor_count_one <- function(id, clData) {
  length(ontologyIndex::get_ancestors(clData, id)) - 1L
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
