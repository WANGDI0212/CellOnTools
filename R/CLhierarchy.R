#' Extract Cell Ontology Hierarchical Structure for Visualization
#'
#' @description
#' Builds a graph-ready node/edge representation of the Cell Ontology subgraph
#' that spans a set of query terms and (optionally) their ancestors.
#'
#' @section Depth convention:
#' Throughout this package, **depth is synonymous with ancestor_count**:
#'
#' \deqn{ancestor\_count(id) = |\{ancestors\}| - 1}
#'
#' The \code{ancestor_count} column in the returned \code{nodes} data frame
#' reflects this definition.  It is \emph{not} the number of edges from the
#' root.  See \code{\link{CLdepth}} for a full explanation.
#'
#' @param ids Character vector of CL IDs to visualize.  Invalid-format or
#'   unknown IDs are skipped with a warning.
#' @param clData An \code{ontology_index} object returned by \code{CLload()}.
#' @param include_ancestors Logical; if \code{TRUE} (default), include ancestors
#'   of the query terms in the graph.
#' @param max_ancestor_count Maximum ancestor_count difference allowed between
#'   the deepest query term and a retained ancestor (default: \code{3}).
#'   Formally, an ancestor \eqn{a} is retained when:
#'   \deqn{ancestor\_count(deepest\_query) - ancestor\_count(a) \le max\_ancestor\_count}
#'   Set to \code{NULL} to include all ancestors.
#'
#' @return A list with two data frames:
#'   \describe{
#'     \item{\code{nodes}}{Columns: \code{id}, \code{label}, \code{is_query},
#'       \code{ancestor_count}.}
#'     \item{\code{edges}}{Columns: \code{from} (child), \code{to} (parent).
#'       Only edges whose both endpoints are in the node set are included.}
#'   }
#'   Compatible with \pkg{igraph} and \pkg{ggraph}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clData <- CLload()
#' ids <- c("CL:0000084", "CL:0000236")
#'
#' hier <- CLhierarchy(ids, clData)
#'
#' # Visualize with igraph
#' library(igraph)
#' g <- graph_from_data_frame(hier$edges, vertices = hier$nodes)
#' plot(g)
#'
#' # Visualize with ggraph
#' library(ggraph)
#' ggraph(g, layout = "sugiyama") +
#'   geom_edge_link(arrow = arrow(length = unit(2, "mm"))) +
#'   geom_node_point(aes(color = is_query), size = 3) +
#'   geom_node_text(aes(label = label), repel = TRUE)
#'
#' # Exclude ancestors for a simpler view
#' CLhierarchy(ids, clData, include_ancestors = FALSE)
#' }
CLhierarchy <- function(ids,
                        clData,
                        include_ancestors  = TRUE,
                        max_ancestor_count = 3L) {

  # ---- Validate inputs ----
  .validate_cldata(clData)

  if (!requireNamespace("ontologyIndex", quietly = TRUE)) {
    stop("Package 'ontologyIndex' is required. Install with:\n",
         "  BiocManager::install('ontologyIndex')", call. = FALSE)
  }

  # Validate IDs - skip (warn) invalid format and unknown IDs
  ids <- as.character(ids)
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (length(ids) == 0L) stop("`ids` must be a non-empty character vector.", call. = FALSE)
  ids <- unique(ids)

  bad_fmt <- ids[!grepl("^CL:\\d+$", ids)]
  if (length(bad_fmt) > 0L) {
    .warn_compact("Invalid CL ID format (skipping)", bad_fmt)
    ids <- setdiff(ids, bad_fmt)
  }

  unknown <- ids[!ids %in% clData$id]
  if (length(unknown) > 0L) {
    .warn_compact("Unknown CL ID(s) (skipping)", unknown)
    ids <- setdiff(ids, unknown)
  }

  if (length(ids) == 0L) {
    stop("No valid, known CL IDs remain after filtering.", call. = FALSE)
  }

  if (!is.null(max_ancestor_count)) {
    if (!is.numeric(max_ancestor_count) || length(max_ancestor_count) != 1L ||
        is.na(max_ancestor_count) || max_ancestor_count < 0) {
      stop("`max_ancestor_count` must be NULL or a non-negative number.", call. = FALSE)
    }
    max_ancestor_count <- as.integer(max_ancestor_count)
  }

  # ---- Determine node set ----
  if (include_ancestors) {
    anc_list <- CLancestors(ids, clData, include_self = FALSE,
                            max_ancestor_count = NULL)
    all_anc  <- unique(unlist(anc_list, use.names = FALSE))

    if (!is.null(max_ancestor_count) && length(all_anc) > 0L) {
      query_counts    <- .get_ancestor_count_vec(ids, clData)
      max_query_count <- max(query_counts)
      anc_counts      <- .get_ancestor_count_vec(all_anc, clData)
      # Keep ancestors within max_ancestor_count steps above the deepest query
      all_anc <- all_anc[max_query_count - anc_counts <= max_ancestor_count]
    }

    nodes <- unique(c(ids, all_anc))
  } else {
    nodes <- ids
  }

  # ---- Build edge list (child -> parent) ----
  edges_list <- lapply(nodes, function(node) {
    parents <- clData$parents[[node]]
    if (is.null(parents) || length(parents) == 0L) return(NULL)
    parents <- intersect(parents, nodes)
    if (length(parents) == 0L) return(NULL)
    data.frame(from = node, to = parents, stringsAsFactors = FALSE)
  })

  edges <- do.call(rbind, Filter(Negate(is.null), edges_list))
  if (is.null(edges)) {
    edges <- data.frame(from = character(0), to = character(0),
                        stringsAsFactors = FALSE)
  }

  # ---- Build node data frame ----
  nodes_df <- data.frame(
    id             = nodes,
    label          = clData$name[nodes],
    is_query       = nodes %in% ids,
    ancestor_count = .get_ancestor_count_vec(nodes, clData),
    stringsAsFactors = FALSE,
    row.names        = NULL
  )

  list(nodes = nodes_df, edges = edges)
}
