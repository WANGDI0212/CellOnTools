# CellOnTools

<!-- badges: start -->
[![R-CMD-check](https://github.com/WANGDI0212/CellOnTools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/WANGDI0212/CellOnTools/actions/workflows/R-CMD-check.yaml)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**CellOnTools** is an R package providing a consistent, well-validated
interface to the [Cell Ontology](https://obofoundry.org/ontology/cl.html) (CL)
together with marker-gene over-representation analysis. It supports loading CL,
converting between identifiers and labels, searching labels, querying ancestors
/ descendants / depth, extracting and plotting hierarchy subgraphs, computing
semantic similarity, rolling terms up to coarser resolutions, mapping free-text
cell-type names to CL via the EBI Ontology Lookup Service, and testing gene sets
for enrichment of cell-type markers using bundled healthy human and mouse
[CellMarkerAccordion](https://github.com/TebaldiLab/cellmarkeraccordion)
annotations.

## Features

| Function | Purpose |
|---|---|
| `CLload()`, `CLdownload()` | Load CL from AnnotationHub or a local OBO file; download an OBO release |
| `CLid2label()`, `CLlabel2id()` | Convert between CL IDs and labels (exact or case-insensitive) |
| `CLsearchLabel()` | Search labels (literal, exact, or regex) |
| `CLancestors()`, `CLdescendants()`, `CLdepth()` | Query ancestry, descendants, and depth (ancestor count) |
| `CLcommonAncestor()` | Find shared (optionally most specific) ancestors |
| `CLhierarchy()`, `CLhierarchyPlot()` | Extract and plot hierarchy subgraphs |
| `CLsimilarity()`, `CLsimilarityMatrix()` | Resnik / Lin semantic similarity |
| `CLrollup()`, `CLsuggestResolution()` | Aggregate terms to coarser resolution; suggest parameters |
| `CLmap()`, `CLmapInteractive()` | Map free-text names to CL terms via OLS |
| `CLmarkers()`, `CLenricher()`, `CLcompareCluster()` | Marker tables and cell-type marker enrichment |

## Installation

Install the released source, or the development version from GitHub:

```r
# from GitHub
# install.packages("remotes")
remotes::install_github("WANGDI0212/CellOnTools")
```

For local development from a source checkout:

```powershell
& "C:\Program Files\R\R-4.4.2\bin\R.exe" CMD INSTALL CellOnTools
```

Several features rely on optional packages, declared under `Suggests` so that
the base install stays lightweight. Install the ones you need:

```r
install.packages(c("ontologyIndex", "ontologySimilarity",
                   "ggraph", "ggplot2", "igraph"))
# Bioconductor packages
# install.packages("BiocManager")
BiocManager::install(c("AnnotationHub", "S4Vectors", "clusterProfiler", "rols"))
```

## Quick start

The bundled marker data work offline — no ontology download required:

```r
library(CellOnTools)

# Bundled marker annotations
human_markers <- CLmarkers("human")
head(human_markers)

# Cell-type marker enrichment for a gene set
res <- CLenricher(c("CD3D", "CD3E", "CD8A", "CD8B", "CD4", "IL7R", "CCR7"),
                  geneType = "symbol", species = "human")
head(as.data.frame(res)[, c("ID", "Description", "p.adjust", "Count")])
```

Ontology operations need a loaded `ontology_index` object:

```r
# Load CL from AnnotationHub, or use CLdownload() + CLload(local_obo = ...)
clData <- CLload(yearAdded = "2023")

CLid2label("CL:0000084", clData)               # "T cell"
CLsearchLabel("T cell", clData, max_results = 5)
CLdepth(c("CL:0000084", "CL:0000236"), clData)
CLcommonAncestor(c("CL:0000084", "CL:0000236"), clData, most_specific = TRUE)
```

See the vignette for a complete walkthrough:

```r
vignette("CellOnTools")
```

## Depth convention

Throughout the package, **depth is the ancestor count** — the number of
distinct proper ancestors of a term (excluding the term itself). The root has
depth 0; more specific terms have larger depths. Because CL is a directed
acyclic graph, this is the cardinality of the transitive ancestor set, *not* the
number of edges to the root. All `max_ancestor_count`-style arguments use this
convention.

## Design

Network-dependent functions (`CLload()`, `CLmap()`, `CLmapInteractive()`) and
those relying on optional packages validate their inputs first and emit clear
dependency or API errors when an external service or package is unavailable, so
failures are actionable rather than cryptic.

## Data provenance and citation

The `CellMarkerAccordion_HumanHealthy` and `CellMarkerAccordion_MouseHealthy`
datasets are derived from the
[CellMarkerAccordion](https://github.com/TebaldiLab/cellmarkeraccordion)
marker-gene resource (healthy collections), harmonised to Cell Ontology terms.
When using the marker functions, please also cite the original resource:

> Busarello E, Biancon G, Cimignolo I, *et al.* Cell Marker Accordion:
> interpretable single-cell and spatial omics annotation in health and disease.
> *Nature Communications* **16**, 5399 (2025).
> doi:[10.1038/s41467-025-60900-4](https://doi.org/10.1038/s41467-025-60900-4)

To cite CellOnTools itself:

```r
citation("CellOnTools")
```

## License

GPL-3. See `DESCRIPTION` for details.
