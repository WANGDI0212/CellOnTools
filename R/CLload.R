#' Load Cell Ontology Data
#'
#' @description
#' Loads the Cell Ontology as an \code{ontology_index} object, either from
#' Bioconductor's AnnotationHub or from a local OBO file downloaded with
#' \code{\link{CLdownload}}.
#'
#' @param yearAdded Year of the Cell Ontology release to load from AnnotationHub
#'   (default: \code{"2023"}).  Pass \code{NULL} to automatically select the
#'   most recent available year.  Ignored when \code{prefer_local = TRUE}.
#' @param local_obo Path to a local \code{.obo} file (default: \code{NULL}).
#'   Used as a fallback when AnnotationHub fails, or as the primary source when
#'   \code{prefer_local = TRUE}.  Recommended for releases not yet indexed in
#'   AnnotationHub - download with \code{\link{CLdownload}}.
#' @param prefer_local Logical; if \code{TRUE}, skip AnnotationHub and load
#'   directly from \code{local_obo} (default: \code{FALSE}).
#' @param verbose Logical; if \code{TRUE} (default), print loading messages.
#'
#' @return An \code{ontology_index} object containing the Cell Ontology, with
#'   at minimum the fields \code{id}, \code{name}, \code{parents}, and
#'   \code{ancestors}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Default: load 2023 release from AnnotationHub
#' clData <- CLload()
#'
#' # Auto-detect the most recent available year
#' clData <- CLload(yearAdded = NULL)
#'
#' # Load a specific year
#' clData <- CLload(yearAdded = "2022")
#'
#' # Load from a local OBO file (recommended for newest releases)
#' CLdownload(dest_file = "cl.obo")
#' clData <- CLload(local_obo = "cl.obo", prefer_local = TRUE)
#' }
CLload <- function(yearAdded    = "2023",
                   local_obo    = NULL,
                   prefer_local = FALSE,
                   verbose      = TRUE) {

  # ---- Validate arguments ----
  if (!is.logical(prefer_local) || length(prefer_local) != 1L || is.na(prefer_local)) {
    stop("`prefer_local` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(yearAdded)) {
    if (!is.character(yearAdded) || length(yearAdded) != 1L || !nzchar(yearAdded)) {
      stop("`yearAdded` must be a non-empty character string (e.g. \"2023\") or NULL.",
           call. = FALSE)
    }
  }
  if (!is.null(local_obo)) {
    if (!is.character(local_obo) || length(local_obo) != 1L) {
      stop("`local_obo` must be a single character string (file path).", call. = FALSE)
    }
    if (!file.exists(local_obo)) {
      stop("local_obo file does not exist: ", local_obo, call. = FALSE)
    }
    if (tolower(tools::file_ext(local_obo)) != "obo") {
      stop("local_obo must be an .obo file.", call. = FALSE)
    }
  }
  if (prefer_local && is.null(local_obo)) {
    stop("`prefer_local = TRUE` requires `local_obo` to be specified.", call. = FALSE)
  }

  onto_obj <- NULL

  # ---- Try AnnotationHub ----
  if (!prefer_local) {
    missing_pkgs <- Filter(
      function(p) !requireNamespace(p, quietly = TRUE),
      c("AnnotationHub", "S4Vectors")
    )
    if (length(missing_pkgs) > 0L) {
      if (verbose) {
        message("Package(s) not available: ",
                paste(missing_pkgs, collapse = ", "),
                ". Will try local OBO file if provided.")
      }
    } else {
      onto_obj <- tryCatch({
        if (verbose) message("Attempting to load Cell Ontology from AnnotationHub...")

        ah  <- AnnotationHub::AnnotationHub()
        opd <- AnnotationHub::query(ah, "ontoProcData")
        meta <- as.data.frame(S4Vectors::mcols(opd))

        # ---- Resolve yearAdded ----
        # Strategy: first filter to Cell Ontology entries, then pick by year.
        # This avoids accidentally picking a non-CL ontology when yearAdded = NULL.
        ontoname <- "cellOnto"

        # Identify CL entries across all years
        is_cl <- grepl(paste0("(^", ontoname, "$|", ontoname, "_)"), meta$title)
        meta_cl <- meta[is_cl, , drop = FALSE]

        if (nrow(meta_cl) == 0L) {
          stop("No Cell Ontology entries found in AnnotationHub.")
        }

        if (is.null(yearAdded)) {
          # Extract four-digit years from rdatadateadded
          years_found <- regmatches(
            meta_cl$rdatadateadded,
            regexpr("\\d{4}", meta_cl$rdatadateadded)
          )
          available_years <- sort(unique(years_found), decreasing = TRUE)
          if (length(available_years) == 0L) {
            stop("Could not determine available years from AnnotationHub metadata.")
          }
          yearAdded <- available_years[1L]
          if (verbose) {
            message("  Auto-detected latest CL year: ", yearAdded,
                    " (available: ", paste(rev(available_years), collapse = ", "), ")")
          }
        }

        # Filter CL entries by year
        year_match <- grepl(yearAdded, meta_cl$rdatadateadded)
        tmp <- meta_cl[year_match, , drop = FALSE]

        if (nrow(tmp) == 0L) {
          stop("Cell Ontology not found for yearAdded = ", yearAdded,
               ". Try yearAdded = NULL to auto-detect.")
        }
        if (nrow(tmp) > 1L) {
          warning("Multiple Cell Ontology entries found for year ", yearAdded,
                  ". Using the first one.", call. = FALSE)
          tmp <- tmp[1L, , drop = FALSE]
        }

        tag    <- rownames(tmp)
        result <- ah[[tag]]

        if (verbose) {
          message("Successfully loaded Cell Ontology from AnnotationHub")
          message("  Year:              ", yearAdded)
          message("  AnnotationHub ID:  ", tag)
          if (!is.null(result$id)) message("  Total terms:       ", length(result$id))
        }

        result

      }, error = function(e) {
        if (verbose) message("AnnotationHub load failed: ", conditionMessage(e))
        NULL
      })
    }
  }

  # ---- Fallback: local OBO file ----
  if (is.null(onto_obj)) {
    if (is.null(local_obo)) {
      stop(
        "Failed to load Cell Ontology from AnnotationHub and no local_obo provided.\n",
        "Tip: download the latest OBO file with CLdownload() and pass the path via local_obo.",
        call. = FALSE
      )
    }

    if (!requireNamespace("ontologyIndex", quietly = TRUE)) {
      stop("Package 'ontologyIndex' is required to load local OBO files.\n",
           "Install with: BiocManager::install('ontologyIndex')", call. = FALSE)
    }

    onto_obj <- tryCatch({
      if (verbose) message("Loading Cell Ontology from local OBO file...")

      # Lightweight OBO header check
      header <- readLines(local_obo, n = 30L, warn = FALSE)
      if (!any(grepl("^format-version:", header)) &&
          !any(grepl("^\\[Term\\]$", header))) {
        warning("File does not appear to be a valid OBO file: ", local_obo,
                call. = FALSE)
      }

      result <- ontologyIndex::get_ontology(local_obo, extract_tags = "everything")

      if (verbose) {
        message("Successfully loaded Cell Ontology from local file")
        message("  File:        ", normalizePath(local_obo))
        if (!is.null(result$id)) message("  Total terms: ", length(result$id))
      }

      result

    }, error = function(e) {
      stop("Failed to load local OBO file: ", local_obo, "\nError: ", e$message,
           call. = FALSE)
    })
  }

  # ---- Validate loaded object ----
  if (is.null(onto_obj)) {
    stop("Failed to load Cell Ontology from any source.", call. = FALSE)
  }
  if (!inherits(onto_obj, "ontology_index")) {
    warning("Loaded object is not of class 'ontology_index'. ",
            "Some package functions may not work correctly.", call. = FALSE)
  }
  missing_fields <- setdiff(c("id", "name", "parents"), names(onto_obj))
  if (length(missing_fields) > 0L) {
    stop("Loaded ontology is missing essential fields: ",
         paste(missing_fields, collapse = ", "), call. = FALSE)
  }

  onto_obj
}
