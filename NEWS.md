# CellOnTools 0.1.0

Initial release.

## Features

* **Ontology loading**: `CLload()` (AnnotationHub or local OBO, with automatic
  fallback) and `CLdownload()` (fetch and validate an OBO release).
* **Identifiers and labels**: `CLid2label()`, `CLlabel2id()` (exact or
  case-insensitive), and `CLsearchLabel()` (literal, exact, or regex search).
* **Graph queries**: `CLancestors()`, `CLdescendants()`, `CLdepth()`,
  `CLcommonAncestor()`, and `CLhierarchy()` / `CLhierarchyPlot()` for subgraph
  extraction and visualisation. Depth is defined consistently as the ancestor
  count of a term.
* **Semantic similarity**: `CLsimilarity()` and `CLsimilarityMatrix()` (Resnik
  and Lin measures via ontologySimilarity).
* **Resolution control**: `CLrollup()` collapses terms onto their most specific
  shared ancestors, and `CLsuggestResolution()` recommends roll-up parameters.
* **Free-text mapping**: `CLmap()` and `CLmapInteractive()` resolve cell-type
  names to CL terms through the EBI Ontology Lookup Service.
* **Marker enrichment**: `CLmarkers()`, `CLenricher()`, and
  `CLcompareCluster()` test gene sets against bundled healthy human and mouse
  CellMarkerAccordion annotations via clusterProfiler.

## Notes

* Heavy and Bioconductor dependencies are declared under `Suggests`; the
  package degrades gracefully with clear, actionable error messages when an
  optional package or network service is unavailable.
* Input validation is applied consistently across all exported functions, with
  a single aggregated warning per problem class (malformed identifiers are
  reported once, not also as "unknown").
