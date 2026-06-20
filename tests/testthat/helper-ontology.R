# ---------------------------------------------------------------------------
# Test fixtures: small, hand-built `ontology_index` objects.
#
# These avoid any network access (AnnotationHub / OLS) or large bundled data,
# so the ontology tests run quickly and deterministically.  The objects expose
# the fields the package relies on: id, name, parents, children, ancestors
# (ancestors include the term itself, matching ontologyIndex conventions).
# ---------------------------------------------------------------------------

# A simple 4-term linear chain: cell -> immune cell -> lymphocyte -> T cell.
test_cl_data <- function() {
  ids <- c("CL:0000000", "CL:0000001", "CL:0000002", "CL:0000003")
  names <- stats::setNames(
    c("cell", "immune cell", "lymphocyte", "T cell"),
    ids
  )
  parents <- list(
    "CL:0000000" = character(0),
    "CL:0000001" = "CL:0000000",
    "CL:0000002" = "CL:0000001",
    "CL:0000003" = "CL:0000002"
  )
  children <- list(
    "CL:0000000" = "CL:0000001",
    "CL:0000001" = "CL:0000002",
    "CL:0000002" = "CL:0000003",
    "CL:0000003" = character(0)
  )
  ancestors <- list(
    "CL:0000000" = "CL:0000000",
    "CL:0000001" = c("CL:0000001", "CL:0000000"),
    "CL:0000002" = c("CL:0000002", "CL:0000001", "CL:0000000"),
    "CL:0000003" = c("CL:0000003", "CL:0000002", "CL:0000001", "CL:0000000")
  )

  structure(
    list(id = ids, name = names, parents = parents, children = children,
         ancestors = ancestors),
    class = "ontology_index"
  )
}

# A small DAG with a multi-parent node (NKT cell has two parents: T cell and
# NK cell).  This exercises directed-acyclic-graph semantics that a linear
# chain cannot:
#
#   cell (000)
#     immune cell (100)
#       lymphocyte (200)
#         T cell (300) ----.
#         B cell (400)     |
#         NK cell (350) --. |
#       myeloid cell (500) \|
#         monocyte (600)   NKT cell (700)   (parents: T cell + NK cell)
test_cl_dag <- function() {
  ids <- c("CL:0000000", "CL:0000100", "CL:0000200", "CL:0000300",
           "CL:0000400", "CL:0000350", "CL:0000500", "CL:0000600",
           "CL:0000700")
  name <- stats::setNames(
    c("cell", "immune cell", "lymphocyte", "T cell", "B cell",
      "NK cell", "myeloid cell", "monocyte", "NKT cell"),
    ids
  )
  parents <- list(
    "CL:0000000" = character(0),
    "CL:0000100" = "CL:0000000",
    "CL:0000200" = "CL:0000100",
    "CL:0000300" = "CL:0000200",
    "CL:0000400" = "CL:0000200",
    "CL:0000350" = "CL:0000200",
    "CL:0000500" = "CL:0000100",
    "CL:0000600" = "CL:0000500",
    "CL:0000700" = c("CL:0000300", "CL:0000350")
  )
  children <- list(
    "CL:0000000" = "CL:0000100",
    "CL:0000100" = c("CL:0000200", "CL:0000500"),
    "CL:0000200" = c("CL:0000300", "CL:0000400", "CL:0000350"),
    "CL:0000300" = "CL:0000700",
    "CL:0000400" = character(0),
    "CL:0000350" = "CL:0000700",
    "CL:0000500" = "CL:0000600",
    "CL:0000600" = character(0),
    "CL:0000700" = character(0)
  )
  ancestors <- list(
    "CL:0000000" = "CL:0000000",
    "CL:0000100" = c("CL:0000100", "CL:0000000"),
    "CL:0000200" = c("CL:0000200", "CL:0000100", "CL:0000000"),
    "CL:0000300" = c("CL:0000300", "CL:0000200", "CL:0000100", "CL:0000000"),
    "CL:0000400" = c("CL:0000400", "CL:0000200", "CL:0000100", "CL:0000000"),
    "CL:0000350" = c("CL:0000350", "CL:0000200", "CL:0000100", "CL:0000000"),
    "CL:0000500" = c("CL:0000500", "CL:0000100", "CL:0000000"),
    "CL:0000600" = c("CL:0000600", "CL:0000500", "CL:0000100", "CL:0000000"),
    "CL:0000700" = c("CL:0000700", "CL:0000300", "CL:0000350",
                     "CL:0000200", "CL:0000100", "CL:0000000")
  )

  structure(
    list(id = ids, name = name, parents = parents, children = children,
         ancestors = ancestors),
    class = "ontology_index"
  )
}

# Capture every warning message raised while evaluating `expr`.
# Returns a character vector (possibly empty); `expr` is still evaluated.
capture_warnings <- function(expr) {
  warns <- character(0)
  withCallingHandlers(
    force(expr),
    warning = function(w) {
      warns[[length(warns) + 1L]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  warns
}
