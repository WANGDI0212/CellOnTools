# CellOnTools 0.1.1

## Compatibility

* The minimum supported R version is now 4.4.0. CI tests R 4.4 explicitly
  with the full dependency set used by the other supported R versions.

## Correctness

* Readable enrichment results now maintain complete S4 object invariants rather
  than changing only the displayed gene strings.
* Custom information-content vectors reject missing, non-finite, negative,
  unnamed, or duplicate values before semantic-similarity calculations.
* Mapping rejects queries that become empty after normalisation, and strict
  integer validation is shared across mapping entry points.
* Interactive mapping keeps global-rank acceptance distinct from page-relative
  selection and preserves `no_match` / `api_error` states when accepting the
  remaining queries.
* AnnotationHub years require an exact four-digit value; cluster names reject
  missing, blank, and duplicate values.

## Reliability and maintainability

* Ontology downloads use a per-destination cross-process lock and promote a
  validated same-directory temporary file through a recoverable rename.
  Network attempts now have configurable per-attempt and total time budgets,
  and waiting processes re-check a populated cache instead of downloading the
  same release again.
* Query normalisation, mapping, validation, and interactive state handling are
  split into smaller internal helpers with regression tests.
* OLS requests and terminal input use testable internal boundaries; interactive
  candidate numbers must now be whole-number text rather than silently
  truncating values such as `1.5`.
* Optional packages used by examples and workflows are declared under
  `Suggests`, and the required testthat version is now accurate.

# CellOnTools 0.1.0

Initial release.

## Breaking API changes during 0.1.0 development

* Graph-distance arguments were renamed to `max_hops` in `CLancestors()`,
  `CLdescendants()`, `CLhierarchy()`, and `CLhierarchyPlot()`. The former
  ancestor-count-difference arguments are not silently mapped to hop distance.
* `CLrollup(max_ancestor_count=)` is now
  `CLrollup(max_candidate_ancestor_count=)`, and
  `CLsuggestResolution(max_ancestor_count_values=)` is now
  `CLsuggestResolution(max_candidate_ancestor_count_values=)`.
* `CLdepth()` now excludes imported non-CL ontology nodes from ancestor count;
  values can therefore be lower than in earlier development snapshots.
* `CLload()` now defaults to the fixed Cell Ontology release `2026-06-08`
  instead of an AnnotationHub year. Explicit `yearAdded` calls retain the
  legacy AnnotationHub route.
* `CLmap()` and `CLmapInteractive()` now stop when OLS serves a different CL
  release, and their results record `ontology_release`.

## Features

* **Ontology loading**: `CLload()` and `CLdownload()` cache, validate, and load
  the fixed `2026-06-08` release; explicit AnnotationHub and local routes remain
  available.
* **Identifiers and labels**: `CLid2label()`, `CLlabel2id()` (exact or
  case-insensitive), and `CLsearchLabel()` (literal, exact, or regex search).
* **Graph queries**: `CLancestors()`, `CLdescendants()`, `CLdepth()`,
  `CLcommonAncestor()`, and `CLhierarchy()` / `CLhierarchyPlot()` for subgraph
  extraction and visualisation. Neighbourhood limits use direct-edge hops,
  while `CLdepth()` reports a separate CL-only ancestor-count specificity
  measure.
* **Semantic similarity**: `CLsimilarity()` and `CLsimilarityMatrix()` (Resnik
  and Lin measures via ontologySimilarity).
* **Resolution control**: `CLrollup()` collapses terms onto their most specific
  shared ancestors, and `CLsuggestResolution()` recommends roll-up parameters.
* **Free-text mapping**: `CLmap()` and `CLmapInteractive()` resolve cell-type
  names to CL terms through the EBI Ontology Lookup Service.
* **Marker enrichment**: `CLmarkers()`, `CLenricher()`, and
  `CLcompareCluster()` test gene sets against bundled healthy human and mouse
  CellMarkerAccordion annotations via clusterProfiler.

## Fixes

* `CLancestors()`, `CLdescendants()`, `CLhierarchy()`, and `CLhierarchyPlot()`
  now use `max_hops` for true direct-edge distance in the CL subgraph, including
  multi-parent DAG nodes.
* Ancestor queries, hierarchy output, common ancestors, and roll-up candidates
  are restricted to `CL:*`; imported BFO/CARO nodes no longer leak into results.
* Ancestor count now counts CL ancestors only and remains explicitly separate
  from graph-hop distance.
* Versioned ontology downloads now verify the `data-version` header and the
  official MD5 before replacing a destination; fallback URLs remain on the
  same fixed tag and never drift to `latest` or `master`.
* Bundled marker tables are harmonised to active `2026-06-08` terms. Six
  obsolete IDs are migrated to their unique official `replaced_by` terms while
  retaining all source rows.
* The bundled marker tables now retain their aggregator repository's MIT
  notice, copyright, source commit, and file checksum in installed notices and
  data attributes; runtime loading validates that provenance metadata.
* `CLrollup()` now uses the explicit `max_candidate_ancestor_count` parameter
  and applies it before pruning redundant ancestors, so an ineligible specific
  term cannot remove the best eligible broader roll-up target. A value of 0 is
  accepted to retain only candidates with no proper CL ancestors; this need not
  identify a unique root.
* `CLsuggestResolution()` uses the corresponding
  `max_candidate_ancestor_count_values` sweep parameter, validates IDs and
  candidate caps up front, sweeps the complete `min_group_size` range from 1
  through the number of input IDs, and no longer returns an all-`NA`
  recommendation when every configuration fails.
* Vignette extraction no longer evaluates optional-dependency expressions or
  network-heavy example code. This prevents older R checks from running
  disabled download and mapping examples while still allowing optional examples
  to run when their packages are installed during a normal vignette build.
* `CLrollup()$groups` now contains only ancestors actually selected by the
  greedy assignment and the terms assigned to each, rather than all remaining
  overlapping candidates.
* `CLrollup()` now rejects malformed IDs, non-finite or fractional count
  parameters, and missing or non-logical control flags with explicit errors.

## Notes

* Heavy and Bioconductor dependencies are declared under `Suggests`; the
  package degrades gracefully with clear, actionable error messages when an
  optional package or network service is unavailable.
* Input validation is applied consistently across all exported functions, with
  a single aggregated warning per problem class (malformed identifiers are
  reported once, not also as "unknown").
