# Deterministically migrate the existing bundled marker tables to the pinned
# Cell Ontology release. This script does not rebuild upstream gene mappings;
# it changes only obsolete CL IDs and canonical CL labels.
#
# Run from the package root:
#   Rscript data-raw/update-marker-ontology-release.R path/to/cl.obo

release <- "2026-06-08"
release_url <- paste0(
  "https://purl.obolibrary.org/obo/cl/releases/",
  release,
  "/cl.obo"
)
release_md5 <- "79fcc8bc4dfa70e5de6d3912bcba1f95"
marker_source_file <- "TheCellMarkerAccordion_database_v1.0.0.xlsx"
marker_source_repository <-
  "https://github.com/TebaldiLab/shiny_cellmarkeraccordion"
marker_source_commit <- "a2cc870a40df2cdd8f2c9671605b19e3f29229d7"
marker_source_sha256 <-
  "53ec885a4e3844c8493d3fb1bb4efde29a8012073b067200d3c9e6b528887857"
marker_repository_license <- "MIT"
marker_repository_copyright <-
  "Copyright (c) 2022 Laboratory of RNA and Disease Data Science (RDDS)"
marker_repository_license_url <- paste0(
  marker_source_repository,
  "/blob/", marker_source_commit, "/LICENSE"
)

args <- commandArgs(trailingOnly = TRUE)
obo_file <- if (length(args) >= 1L) args[1L] else Sys.getenv("CELLONTOOLS_CL_OBO")
if (!nzchar(obo_file) || !file.exists(obo_file)) {
  stop(
    "Provide the 2026-06-08 cl.obo path as the first argument or via ",
    "CELLONTOOLS_CL_OBO."
  )
}
if (!requireNamespace("ontologyIndex", quietly = TRUE)) {
  stop("Package 'ontologyIndex' is required.")
}

actual_md5 <- tolower(unname(tools::md5sum(obo_file)))
if (!identical(actual_md5, release_md5)) {
  stop("Unexpected cl.obo checksum: ", actual_md5)
}

cl <- ontologyIndex::get_ontology(obo_file, extract_tags = "everything")
version_line <- grep("^data-version:", attr(cl, "version"), value = TRUE)
if (length(version_line) != 1L ||
    !grepl(paste0("releases/", release, "$"), version_line)) {
  stop("cl.obo is not Cell Ontology release ", release, ".")
}

expected_replacements <- c(
  "CL:0000402" = "CL:0000099",
  "CL:4023083" = "CL:4023036",
  "CL:0000555" = "CL:4023161",
  "CL:4023070" = "CL:4023064",
  "CL:0010003" = "CL:0000322",
  "CL:0000651" = "CL:0002181"
)

harmonise_one <- function(data, expected_rows) {
  stopifnot(nrow(data) == expected_rows)

  ids <- unique(data$CL_ID)
  if (any(!grepl("^CL:\\d+$", ids)) || any(!ids %in% cl$id)) {
    stop("Marker table contains invalid or absent CL IDs.")
  }

  obsolete <- ids[unname(cl$obsolete[ids]) %in% TRUE]
  if (!setequal(obsolete, intersect(ids, names(expected_replacements)))) {
    stop("Unexpected obsolete marker terms: ", paste(obsolete, collapse = ", "))
  }

  if (length(obsolete) > 0L) {
    actual_replacements <- vapply(
      obsolete,
      function(id) {
        replacement <- cl$replaced_by[[id]]
        if (length(replacement) != 1L) {
          stop("Obsolete term does not have exactly one replaced_by: ", id)
        }
        replacement
      },
      character(1L)
    )
    if (!identical(
      unname(actual_replacements),
      unname(expected_replacements[names(actual_replacements)])
    )) {
      stop("Pinned replacement map no longer matches the OBO file.")
    }

    idx <- match(data$CL_ID, names(actual_replacements))
    replace <- !is.na(idx)
    data$CL_ID[replace] <- unname(actual_replacements[idx[replace]])
  }

  active <- grepl("^CL:\\d+$", data$CL_ID) &
    data$CL_ID %in% cl$id &
    !(unname(cl$obsolete[data$CL_ID]) %in% TRUE)
  if (!all(active)) stop("Marker migration left non-active CL IDs.")

  canonical_labels <- unname(cl$name[data$CL_ID])
  if (anyNA(canonical_labels) || any(!nzchar(canonical_labels))) {
    stop("Could not resolve canonical labels for all migrated CL IDs.")
  }
  data$CL_label <- canonical_labels

  attr(data, "ontology_release") <- release
  attr(data, "ontology_url") <- release_url
  attr(data, "ontology_md5") <- release_md5
  attr(data, "ontology_obsolete_replacements") <- expected_replacements
  attr(data, "marker_source_file") <- marker_source_file
  attr(data, "marker_source_repository") <- marker_source_repository
  attr(data, "marker_source_commit") <- marker_source_commit
  attr(data, "marker_source_sha256") <- marker_source_sha256
  attr(data, "marker_source_license") <- NULL
  attr(data, "marker_source_copyright") <- NULL
  attr(data, "marker_source_license_url") <- NULL
  attr(data, "marker_repository_license") <- marker_repository_license
  attr(data, "marker_repository_copyright") <- marker_repository_copyright
  attr(data, "marker_repository_license_url") <-
    marker_repository_license_url
  data
}

objects <- c(
  CellMarkerAccordion_HumanHealthy = 140337L,
  CellMarkerAccordion_MouseHealthy = 49289L
)

for (obj_name in names(objects)) {
  path <- file.path("data", paste0(obj_name, ".rda"))
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  data <- get(obj_name, envir = env, inherits = FALSE)
  data <- harmonise_one(data, objects[[obj_name]])
  assign(obj_name, data, envir = env)
  save(list = obj_name, file = path, envir = env, compress = "xz")
  message("Updated ", path)
}
