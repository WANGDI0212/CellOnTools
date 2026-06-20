## Test environments

* Local: Windows 10, R 4.4.2
* GitHub Actions: macOS, Windows, Ubuntu (R-release, R-devel, R-oldrel)

## R CMD check results

0 errors | 0 warnings | 0 notes

(A transient "unable to verify current time" NOTE may appear when the check
machine has no network access to a time server; it is unrelated to the package.)

## Notes for reviewers

* Functionality that depends on external services (AnnotationHub, the EBI
  Ontology Lookup Service) or on heavier Bioconductor packages
  (clusterProfiler, rols, AnnotationHub, S4Vectors) is placed under `Suggests`.
  All such functions guard their dependencies with `requireNamespace()` and
  fail with an informative message; their examples are wrapped in `\dontrun{}`.
* The bundled `CellMarkerAccordion_*` datasets are derived from the
  CellMarkerAccordion marker-gene resource (healthy collections), harmonised to
  Cell Ontology terms.
