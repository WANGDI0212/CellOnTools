# CellOnTools

<!-- badges: start -->
[![R-CMD-check](https://github.com/WANGDI0212/CellOnTools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/WANGDI0212/CellOnTools/actions/workflows/R-CMD-check.yaml)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**CellOnTools** is an R package for making cell-type annotations easier to
audit, compare, and reuse with the
[Cell Ontology](https://obofoundry.org/ontology/cl.html) (CL). It provides a
consistent interface for loading CL, converting between CL identifiers and
labels, searching and mapping cell-type names, exploring ontology hierarchy,
computing semantic similarity, rolling fine-grained annotations to coarser
terms, and testing gene sets for enrichment of bundled healthy human and mouse
[CellMarkerAccordion](https://github.com/TebaldiLab/cellmarkeraccordion)
markers.

The package is especially useful when you want to turn single-cell cluster
labels such as `"Naive CD4 T"`, `"CD14+ Mono"`, or `"NK"` into explicit CL IDs,
then carry those IDs through downstream plots, metadata tables, enrichment
checks, and cross-dataset comparisons.

## What You Can Do

| Task | Main functions |
|---|---|
| Load Cell Ontology releases | `CLload()`, `CLdownload()` |
| Convert between CL IDs and labels | `CLid2label()`, `CLlabel2id()` |
| Search or map cell-type names | `CLsearchLabel()`, `CLmap()`, `CLmapInteractive()` |
| Inspect ontology relationships | `CLancestors()`, `CLdescendants()`, `CLdepth()`, `CLcommonAncestor()` |
| Build and plot hierarchy subgraphs | `CLhierarchy()`, `CLhierarchyPlot()` |
| Compare terms semantically | `CLsimilarity()`, `CLsimilarityMatrix()` |
| Harmonise annotations to broader terms | `CLrollup()`, `CLsuggestResolution()` |
| Use marker-gene resources | `CLmarkers()`, `CLenricher()`, `CLcompareCluster()` |

## Installation

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("WANGDI0212/CellOnTools")
```

For local development from a source checkout:

```powershell
& "C:\Program Files\R\R-4.4.2\bin\R.exe" CMD INSTALL CellOnTools
```

CellOnTools keeps several heavier packages in `Suggests` so that the base
install stays lightweight. Install only the pieces needed for your workflow:

| Workflow | Suggested packages |
|---|---|
| Local OBO loading | `ontologyIndex` |
| AnnotationHub loading | `AnnotationHub`, `S4Vectors` |
| Semantic similarity | `ontologySimilarity` |
| Hierarchy plots | `ggraph`, `ggplot2`, `igraph` |
| OLS name mapping | `rols` |
| Marker enrichment | `clusterProfiler` |

To install the full workflow stack:

```r
install.packages(c("ontologyIndex", "ontologySimilarity",
                   "ggraph", "ggplot2", "igraph"))

# install.packages("BiocManager")
BiocManager::install(c("AnnotationHub", "S4Vectors",
                       "clusterProfiler", "rols"))
```

## Quick Start

The bundled marker tables work offline and do not require downloading the
ontology:

```r
library(CellOnTools)

human_markers <- CLmarkers("human")
head(human_markers)

data.frame(
  species = "human",
  marker_rows = nrow(human_markers),
  unique_cl_terms = length(unique(human_markers$CL_ID)),
  unique_marker_symbols = length(unique(human_markers$marker_symbol))
)
```

To run enrichment, install `clusterProfiler` first:

```r
if (requireNamespace("clusterProfiler", quietly = TRUE)) {
  t_cell_genes <- c("CD3D", "CD3E", "CD8A", "CD8B", "CD4", "IL7R", "CCR7")

  res <- CLenricher(t_cell_genes, geneType = "symbol", species = "human")

  if (!is.null(res)) {
    head(as.data.frame(res)[, c("ID", "Description", "p.adjust", "Count")])
  }
}
```

Ontology operations use a loaded `ontology_index` object. Load from
AnnotationHub when available, or download a reproducible local OBO file:

```r
# From AnnotationHub:
clData <- CLload(yearAdded = "2023")

# Or from a local OBO file:
CLdownload(dest_file = "cl.obo")
clData <- CLload(local_obo = "cl.obo", prefer_local = TRUE)

CLid2label("CL:0000084", clData)
CLsearchLabel("T cell", clData, max_results = 5)
CLdepth(c("CL:0000084", "CL:0000236"), clData)
CLcommonAncestor(c("CL:0000084", "CL:0000236"), clData, most_specific = TRUE)
```

## Single-Cell Annotation Pattern

A common workflow is to keep the original cluster labels, add reviewed CL IDs,
and then use those CL IDs for hierarchy, similarity, and roll-up operations:

```r
pbmc_terms <- data.frame(
  seurat_label = c("Naive CD4 T", "Memory CD4 T", "CD14+ Mono", "B",
                   "CD8 T", "FCGR3A+ Mono", "NK", "DC", "Platelet"),
  reviewed_cl_label = c(
    "naive thymus-derived CD4-positive, alpha-beta T cell",
    "CD4-positive, alpha-beta memory T cell",
    "CD14-positive monocyte",
    "B cell",
    "CD8-positive, alpha-beta T cell",
    "CD14-low, CD16-positive monocyte",
    "natural killer cell",
    "dendritic cell",
    "platelet"
  )
)

pbmc_terms$cl_id    <- CLlabel2id(pbmc_terms$reviewed_cl_label, clData,
                                  strict = TRUE)
pbmc_terms$cl_label <- CLid2label(pbmc_terms$cl_id, clData)
pbmc_terms$cl_depth <- CLdepth(pbmc_terms$cl_id, clData)
```

From the pbmc3k case study used to guide the package documentation, all 9
Seurat annotation labels were resolved to CL IDs. The same analysis loaded
18,460 CL terms, exported CL metadata for 2,638 cells, and rolled the 9 reviewed
annotations into 3 broader groups:

| Seurat label | CL ID | CL label | Roll-up label |
|---|---|---|---|
| Naive CD4 T | `CL:0000895` | naive thymus-derived CD4-positive, alpha-beta T cell | lymphocyte |
| Memory CD4 T | `CL:0000897` | CD4-positive, alpha-beta memory T cell | lymphocyte |
| CD14+ Mono | `CL:0001054` | CD14-positive monocyte | mononuclear phagocyte |
| B | `CL:0000236` | B cell | lymphocyte |
| CD8 T | `CL:0000625` | CD8-positive, alpha-beta T cell | lymphocyte |
| FCGR3A+ Mono | `CL:0002396` | CD14-low, CD16-positive monocyte | mononuclear phagocyte |
| NK | `CL:0000623` | natural killer cell | lymphocyte |
| DC | `CL:0000451` | dendritic cell | mononuclear phagocyte |
| Platelet | `CL:0000233` | platelet | platelet |

## Marker Enrichment As Evidence

`CLenricher()` and `CLcompareCluster()` are best read as marker-based evidence,
not as a replacement for reviewing cell-type annotations. In the pbmc3k
workflow, `CLcompareCluster()` produced 674 enriched rows from 3,456 Seurat
marker rows. The top enriched CL term was an exact ID match for 1 of 9 reviewed
annotations, but the median Lin semantic similarity between the top enriched
term and the reviewed CL term was 0.790. That is a useful sanity check: marker
enrichment often supports the same biological neighbourhood even when the
highest-ranked marker term is more general, more specific, or based on a
different naming convention.

```r
gene_clusters <- list(
  "Naive CD4 T" = c("IL7R", "CCR7", "LTB", "CD3D", "CD3E"),
  "B"          = c("MS4A1", "CD79A", "CD79B", "CD74"),
  "NK"         = c("NKG7", "GNLY", "PRF1", "GZMB")
)

if (requireNamespace("clusterProfiler", quietly = TRUE)) {
  cc <- CLcompareCluster(gene_clusters, geneType = "symbol",
                         species = "human")
  head(as.data.frame(cc)[, c("Cluster", "ID", "Description", "p.adjust")])
}
```

## Vignettes

Start with the package overview:

```r
vignette("CellOnTools", package = "CellOnTools")
```

For a manuscript-style single-cell workflow inspired by the pbmc3k analysis:

```r
vignette("pbmc3k-workflow", package = "CellOnTools")
```

## Depth Convention

Throughout the package, **depth is the ancestor count**: the number of distinct
proper ancestors of a term, excluding the term itself. The root has depth 0;
more specific terms have larger depths. Because CL is a directed acyclic graph,
this is the cardinality of the transitive ancestor set, not the number of edges
to one chosen root path. All `max_ancestor_count`-style arguments use this
convention.

## Reproducibility Notes

Network-dependent functions (`CLload()`, `CLmap()`, `CLmapInteractive()`) and
functions relying on optional packages validate inputs first and return
actionable dependency or API errors. For scripted analyses, prefer a local OBO
file for reproducible CL loading, guard OLS calls with `tryCatch()`, and export
intermediate mapping, enrichment, and roll-up tables alongside figures.

## Data Provenance And Citation

The `CellMarkerAccordion_HumanHealthy` and `CellMarkerAccordion_MouseHealthy`
datasets are derived from the
[CellMarkerAccordion](https://github.com/TebaldiLab/cellmarkeraccordion)
marker-gene resource (healthy collections), harmonised to Cell Ontology terms.
When using the marker functions, please also cite the original resource:

> Busarello E, Biancon G, Cimignolo I, et al. Cell Marker Accordion:
> interpretable single-cell and spatial omics annotation in health and disease.
> Nature Communications 16, 5399 (2025).
> doi:[10.1038/s41467-025-60900-4](https://doi.org/10.1038/s41467-025-60900-4)

To cite CellOnTools itself:

```r
citation("CellOnTools")
```

## License

GPL-3. See `DESCRIPTION` for details.
