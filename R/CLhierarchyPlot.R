#' Plot Cell Ontology Hierarchy
#'
#' @description
#' Visualizes the Cell Ontology subgraph returned by \code{\link{CLhierarchy}}
#' using \pkg{ggraph}.  Query terms are highlighted in a distinct colour.
#'
#' @section Depth convention:
#' Node colouring and filtering use \strong{ancestor_count} (not root-distance).
#' See \code{\link{CLdepth}} for the package-wide definition.
#'
#' @param ids Character vector of CL IDs to visualize.
#' @param clData An \code{ontology_index} object returned by \code{CLload()}.
#' @param include_ancestors Logical; if \code{TRUE} (default), include ancestors.
#' @param max_ancestor_count Maximum ancestor_count difference above the deepest
#'   query term (default: \code{3}).  Passed to \code{\link{CLhierarchy}}.
#' @param layout Graph layout algorithm: \code{"sugiyama"} (default, recommended
#'   for DAGs), \code{"tree"}, \code{"fr"}, or \code{"kk"}.
#' @param node_size Size of nodes (default: \code{3}).
#' @param label_size Size of text labels (default: \code{3}).
#' @param query_color Colour for query nodes (default: \code{"red"}).
#' @param ancestor_color Colour for ancestor nodes (default: \code{"lightblue"}).
#' @param show_id Logical; if \code{TRUE} (default), display both CL ID and
#'   label; if \code{FALSE}, display label only.
#' @param id_label_sep Separator between ID and label when \code{show_id = TRUE}
#'   (default: \code{"\n"}).
#'
#' @return A \code{ggplot} object, or \code{NULL} (invisibly) if the hierarchy
#'   contains no edges (with a warning).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clData <- CLload()
#' ids <- c("CL:0000084", "CL:0000236")
#'
#' # Default plot
#' CLhierarchyPlot(ids, clData)
#'
#' # Label only, custom colours
#' CLhierarchyPlot(ids, clData, show_id = FALSE,
#'                 query_color = "darkred", ancestor_color = "steelblue")
#' }
CLhierarchyPlot <- function(ids,
                            clData,
                            include_ancestors  = TRUE,
                            max_ancestor_count = 3L,
                            layout             = c("sugiyama", "tree", "fr", "kk"),
                            node_size          = 3,
                            label_size         = 3,
                            query_color        = "red",
                            ancestor_color     = "lightblue",
                            show_id            = TRUE,
                            id_label_sep       = "\n") {

  # ---- Check dependencies ----
  for (pkg in c("ggraph", "igraph", "ggplot2")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required. Install with:\n",
           "  install.packages('", pkg, "')", call. = FALSE)
    }
  }

  layout <- match.arg(layout)

  # ---- Extract hierarchy ----
  hierarchy <- CLhierarchy(
    ids                = ids,
    clData             = clData,
    include_ancestors  = include_ancestors,
    max_ancestor_count = max_ancestor_count
  )

  # ---- Guard: no edges ----
  if (nrow(hierarchy$edges) == 0L) {
    warning("No edges found in hierarchy. Cannot create plot.", call. = FALSE)
    return(invisible(NULL))
  }

  # ---- Build display labels ----
  hierarchy$nodes$display_label <- if (show_id) {
    paste0(hierarchy$nodes$id, id_label_sep, "(", hierarchy$nodes$label, ")")
  } else {
    hierarchy$nodes$label
  }

  # ---- Create igraph object ----
  g <- igraph::graph_from_data_frame(
    d        = hierarchy$edges,
    vertices = hierarchy$nodes,
    directed = TRUE
  )

  # ---- Plot ----
  p <- ggraph::ggraph(g, layout = layout) +
    ggraph::geom_edge_link(
      arrow   = grid::arrow(length = grid::unit(2, "mm")),
      end_cap = ggraph::circle(3, "mm"),
      color   = "gray50",
      alpha   = 0.6
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(color = is_query),
      size = node_size
    ) +
    ggplot2::scale_color_manual(
      values = c("TRUE" = query_color, "FALSE" = ancestor_color),
      labels = c("TRUE" = "Query",     "FALSE" = "Ancestor"),
      name   = "Node type"
    ) +
    ggraph::geom_node_text(
      ggplot2::aes(label = display_label),
      size  = label_size,
      repel = TRUE
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title      = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold")
    ) +
    ggplot2::labs(
      title    = "Cell Ontology Hierarchy",
      subtitle = if (is.null(max_ancestor_count)) {
        "Showing all ancestors of the query terms"
      } else {
        paste0("Depth filter: ancestor_count \u2264 ", max_ancestor_count,
               " above deepest query")
      }
    )

  p
}
