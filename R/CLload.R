#' Load Cell Ontology Data
#'
#' @description
#' Loads the Cell Ontology as an \code{ontology_index} object. By default,
#' \code{CLload()} downloads (once) and caches the fixed \code{2026-06-08}
#' release. Explicit \code{yearAdded} calls retain the legacy AnnotationHub
#' route, and local OBO files can be used as either a fallback or the primary
#' source.
#'
#' @param yearAdded Year used to select a Cell Ontology entry from
#'   AnnotationHub. Omit this argument to use the package's fixed release.
#'   Supply a four-digit year (for example, \code{"2023"}) for a specific
#'   AnnotationHub entry, or explicitly pass \code{NULL} to select the most
#'   recent Cell Ontology entry available in AnnotationHub.
#' @param local_obo Path to a local \code{.obo} file (default: \code{NULL}).
#'   Used as a fallback when the selected network source fails, or as the
#'   primary source when \code{prefer_local = TRUE}.
#' @param prefer_local Logical; if \code{TRUE}, load directly from
#'   \code{local_obo} and do not contact a network source (default:
#'   \code{FALSE}).
#' @param verbose Logical; if \code{TRUE} (default), print loading messages.
#' @param release Versioned Cell Ontology release date used when
#'   \code{yearAdded} is omitted (default: \code{"2026-06-08"}). The release
#'   is cached under \code{tools::R_user_dir("CellOnTools", "cache")}; set
#'   option \code{CellOnTools.cache_dir} to use a different cache directory.
#'
#' @return An \code{ontology_index} object containing the Cell Ontology, with
#'   at minimum the fields \code{id}, \code{name}, \code{parents},
#'   \code{children}, and \code{ancestors}. Attributes
#'   \code{ontology_release} and \code{ontology_source} record the actual
#'   loaded release and source.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Default: fixed and checksum-verified 2026-06-08 release
#' clData <- CLload()
#'
#' # Legacy AnnotationHub routes remain available when requested explicitly
#' clData_2023 <- CLload(yearAdded = "2023")
#' clData_ah_latest <- CLload(yearAdded = NULL)
#'
#' # Load a local OBO file without contacting a network source
#' CLdownload(dest_file = "cl.obo")
#' clData_local <- CLload(
#'   local_obo = "cl.obo",
#'   prefer_local = TRUE
#' )
#' }
CLload <- function(yearAdded    = NULL,
                   local_obo    = NULL,
                   prefer_local = FALSE,
                   verbose      = TRUE,
                   release      = "2026-06-08") {

  year_was_missing <- missing(yearAdded)
  release_was_missing <- missing(release)

  # ---- Validate arguments ----
  prefer_local <- .validate_logical_scalar(prefer_local, "prefer_local")
  verbose <- .validate_logical_scalar(verbose, "verbose")

  if (!year_was_missing && !is.null(yearAdded)) {
    if (!is.character(yearAdded) || length(yearAdded) != 1L ||
        is.na(yearAdded) || !grepl("^[0-9]{4}$", yearAdded)) {
      stop(
        "`yearAdded` must be a four-digit year string (e.g. \"2023\") ",
        "or NULL.",
        call. = FALSE
      )
    }
  }

  if (!is.character(release) || length(release) != 1L || is.na(release) ||
      !grepl("^\\d{4}-\\d{2}-\\d{2}$", release)) {
    stop("`release` must be a date string in YYYY-MM-DD format.", call. = FALSE)
  }

  if (!prefer_local && !year_was_missing && !release_was_missing) {
    stop(
      "Specify only one of `yearAdded` (AnnotationHub) or `release` ",
      "(versioned OBO).",
      call. = FALSE
    )
  }

  if (!is.null(local_obo)) {
    if (!is.character(local_obo) || length(local_obo) != 1L ||
        is.na(local_obo) || !nzchar(local_obo)) {
      stop(
        "`local_obo` must be a single non-empty character string (file path).",
        call. = FALSE
      )
    }
    if (!file.exists(local_obo)) {
      stop("local_obo file does not exist: ", local_obo, call. = FALSE)
    }
    if (tolower(tools::file_ext(local_obo)) != "obo") {
      stop("local_obo must be an .obo file.", call. = FALSE)
    }
  }
  if (prefer_local && is.null(local_obo)) {
    stop("`prefer_local = TRUE` requires `local_obo` to be specified.",
         call. = FALSE)
  }

  # ---- Explicit local source ----
  if (prefer_local) {
    return(.load_cl_obo_file(
      local_obo,
      verbose = verbose,
      source = normalizePath(local_obo, winslash = "/", mustWork = TRUE)
    ))
  }

  # Omitting yearAdded selects the fixed, versioned OBO route. Explicitly
  # supplying yearAdded (including NULL) selects the legacy AnnotationHub route.
  source_mode <- if (year_was_missing) "release" else "annotationhub"
  primary_error <- NULL

  onto_obj <- tryCatch(
    {
      if (identical(source_mode, "release")) {
        .load_cl_release(release = release, verbose = verbose)
      } else {
        .load_cl_annotationhub(yearAdded = yearAdded, verbose = verbose)
      }
    },
    error = function(e) {
      primary_error <<- conditionMessage(e)
      if (verbose) {
        message(
          if (identical(source_mode, "release")) {
            "Versioned OBO load failed: "
          } else {
            "AnnotationHub load failed: "
          },
          primary_error
        )
      }
      NULL
    }
  )

  # ---- Optional local fallback ----
  if (is.null(onto_obj) && !is.null(local_obo)) {
    if (verbose) message("Trying the supplied local OBO fallback...")
    onto_obj <- .load_cl_obo_file(
      local_obo,
      verbose = verbose,
      expected_release = if (identical(source_mode, "release")) release else NULL,
      source = normalizePath(local_obo, winslash = "/", mustWork = TRUE)
    )
  }

  if (is.null(onto_obj)) {
    stop(
      "Failed to load Cell Ontology from the selected source",
      if (!is.null(primary_error)) paste0(": ", primary_error) else "",
      ".\nProvide `local_obo` as a fallback, or set `prefer_local = TRUE` ",
      "to use a local file directly.",
      call. = FALSE
    )
  }

  .validate_cldata(onto_obj)
  onto_obj
}

.load_cl_release <- function(release = .CL_RELEASE, verbose = TRUE) {
  release <- .normalise_cl_release(release)
  if (is.na(release)) {
    stop("`release` must contain a date in YYYY-MM-DD format.", call. = FALSE)
  }

  cache_file <- .cl_cache_file(release)
  cache_dir <- dirname(cache_file)
  if (!dir.exists(cache_dir) &&
      !dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create CellOnTools cache directory: ", cache_dir,
         call. = FALSE)
  }

  expected_md5 <- .cl_release_md5(release)
  cache_error <- NULL
  validate_cache <- function() {
    cache_error <<- NULL
    if (!file.exists(cache_file)) return(FALSE)
    tryCatch(
      {
        .validate_cl_obo_file(
          cache_file,
          expected_release = release,
          expected_md5 = expected_md5
        )
        TRUE
      },
      error = function(e) {
        cache_error <<- conditionMessage(e)
        FALSE
      }
    )
  }

  cache_valid <- validate_cache()
  if (!cache_valid) {
    # Re-check only after acquiring the same lock used by CLdownload(). Another
    # process may have populated the cache while this process was waiting.
    lock_dir <- .acquire_cl_file_lock(cache_file)
    tryCatch(
      {
        cache_valid <- validate_cache()
        if (!cache_valid) {
          if (verbose) {
            if (file.exists(cache_file)) {
              message("Cached Cell Ontology file failed validation: ", cache_error)
            }
            message("Downloading fixed Cell Ontology release ", release, "...")
          }
          .download_cl_obo_locked(
            dest_file = cache_file,
            url = .cl_release_urls(release)[1L],
            overwrite = TRUE
          )
          cache_valid <- TRUE
        } else if (verbose) {
          message(
            "Loading cached Cell Ontology release ", release,
            " populated by another process..."
          )
        }
      },
      finally = .release_cl_file_lock(lock_dir)
    )
  } else if (verbose) {
    message("Loading cached Cell Ontology release ", release, "...")
  }

  .load_cl_obo_file(
    cache_file,
    verbose = verbose,
    expected_release = release,
    expected_md5 = expected_md5,
    source = normalizePath(cache_file, winslash = "/", mustWork = TRUE)
  )
}

.load_cl_obo_file <- function(path,
                              verbose = TRUE,
                              expected_release = NULL,
                              expected_md5 = NULL,
                              source = path) {
  if (!requireNamespace("ontologyIndex", quietly = TRUE)) {
    stop(
      "Package 'ontologyIndex' is required to load OBO files. Install with:\n",
      "  BiocManager::install('ontologyIndex')",
      call. = FALSE
    )
  }

  .validate_cl_obo_file(
    path,
    expected_release = expected_release,
    expected_md5 = expected_md5
  )

  if (verbose) message("Loading Cell Ontology from OBO file...")
  result <- tryCatch(
    ontologyIndex::get_ontology(path, extract_tags = "everything"),
    error = function(e) {
      stop(
        "Failed to parse Cell Ontology OBO file: ", path,
        "\nError: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

  if (!inherits(result, "ontology_index")) {
    stop("Loaded object is not of class 'ontology_index'.", call. = FALSE)
  }
  .validate_cldata(result)
  result <- .annotate_cl_release(result, source = source)

  if (verbose) {
    message("Successfully loaded Cell Ontology")
    message("  Release:    ", attr(result, "ontology_release"))
    message("  Total IDs:  ", length(result$id))
    message("  CL IDs:     ", sum(grepl("^CL:\\d+$", result$id)))
    message("  Source:     ", source)
  }

  result
}

.load_cl_annotationhub <- function(yearAdded = NULL, verbose = TRUE) {
  missing_pkgs <- Filter(
    function(p) !requireNamespace(p, quietly = TRUE),
    c("AnnotationHub", "S4Vectors")
  )
  if (length(missing_pkgs) > 0L) {
    stop(
      "Package(s) required for AnnotationHub loading are not available: ",
      paste(missing_pkgs, collapse = ", "),
      call. = FALSE
    )
  }

  if (verbose) message("Attempting to load Cell Ontology from AnnotationHub...")
  ah <- AnnotationHub::AnnotationHub()
  opd <- AnnotationHub::query(ah, "ontoProcData")
  meta <- as.data.frame(S4Vectors::mcols(opd))

  ontoname <- "cellOnto"
  is_cl <- grepl(paste0("(^", ontoname, "$|", ontoname, "_)"), meta$title)
  meta_cl <- meta[is_cl, , drop = FALSE]
  if (nrow(meta_cl) == 0L) {
    stop("No Cell Ontology entries found in AnnotationHub.", call. = FALSE)
  }

  entry_years <- vapply(
    as.character(meta_cl$rdatadateadded),
    function(value) {
      if (is.na(value) || !nzchar(value)) return(NA_character_)
      match <- regexpr("(?<![0-9])[0-9]{4}(?![0-9])", value, perl = TRUE)
      if (match[1L] < 0L) return(NA_character_)
      regmatches(value, match)
    },
    character(1L),
    USE.NAMES = FALSE
  )

  selected_year <- yearAdded
  if (is.null(selected_year)) {
    available_years <- sort(
      unique(entry_years[!is.na(entry_years)]),
      decreasing = TRUE
    )
    if (length(available_years) == 0L) {
      stop("Could not determine available years from AnnotationHub metadata.",
           call. = FALSE)
    }
    selected_year <- available_years[1L]
    if (verbose) {
      message(
        "  Auto-detected latest AnnotationHub CL year: ", selected_year,
        " (available: ", paste(rev(available_years), collapse = ", "), ")"
      )
    }
  }

  year_match <- !is.na(entry_years) & entry_years == selected_year
  tmp <- meta_cl[year_match, , drop = FALSE]
  if (nrow(tmp) == 0L) {
    stop(
      "Cell Ontology not found for yearAdded = ", selected_year,
      ". Explicitly pass yearAdded = NULL to auto-detect.",
      call. = FALSE
    )
  }
  if (nrow(tmp) > 1L) {
    warning(
      "Multiple Cell Ontology entries found for year ", selected_year,
      ". Using the first one.",
      call. = FALSE
    )
    tmp <- tmp[1L, , drop = FALSE]
  }

  tag <- rownames(tmp)
  result <- ah[[tag]]
  if (!inherits(result, "ontology_index")) {
    stop("Loaded AnnotationHub object is not an ontology_index.", call. = FALSE)
  }
  .validate_cldata(result)
  result <- .annotate_cl_release(result, source = paste0("AnnotationHub:", tag))

  if (verbose) {
    message("Successfully loaded Cell Ontology from AnnotationHub")
    message("  AnnotationHub year: ", selected_year)
    message("  Ontology release:   ", attr(result, "ontology_release"))
    message("  AnnotationHub ID:   ", tag)
    message("  Total IDs:          ", length(result$id))
  }

  result
}
