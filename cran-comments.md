## Test environments

* Local: Windows 10, R 4.4.2
* Minimum-version CI: Ubuntu, R 4.4
* The configured GitHub Actions matrix covers macOS/Windows/Ubuntu,
  R-devel/release/oldrel, and the declared minimum R 4.4.

## R CMD check results

0 errors | 0 warnings | 2 notes

* `New submission`
* `unable to verify current time`

The second NOTE is environmental: the check machine could not contact a time
server. Installed-package tests completed with 382 passes, no failures, no
warnings, and no skips under R 4.4.2.

The incoming-feasibility check also reported connection resets/timeouts for
three GitHub URLs belonging to the cited CellMarkerAccordion data source. All
three URLs were independently confirmed reachable on 2026-07-14 and are kept
for data provenance.

## Notes for reviewers

* Functionality that depends on external services (AnnotationHub, the EBI
  Ontology Lookup Service) or on heavier Bioconductor packages
  (clusterProfiler, rols, AnnotationHub, S4Vectors) is placed under `Suggests`.
  All such functions guard their dependencies with `requireNamespace()` and
  fail with an informative message; their examples are wrapped in `\dontrun{}`.
* The bundled `CellMarkerAccordion_*` datasets are derived from the
  CellMarkerAccordion marker-gene resource (healthy collections), harmonised to
  Cell Ontology terms.
