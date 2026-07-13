# ============================================================================
# utils-release.R
# Internal release metadata and compatibility checks for Cell Ontology.
# ============================================================================

.CL_RELEASE <- "2026-06-08"
.CL_RELEASE_URL <- paste0(
  "https://purl.obolibrary.org/obo/cl/releases/",
  .CL_RELEASE,
  "/cl.obo"
)
.CL_RELEASE_MD5 <- "79fcc8bc4dfa70e5de6d3912bcba1f95"
.CL_MARKER_REPLACEMENTS <- c(
  "CL:0000402" = "CL:0000099",
  "CL:4023083" = "CL:4023036",
  "CL:0000555" = "CL:4023161",
  "CL:4023070" = "CL:4023064",
  "CL:0010003" = "CL:0000322",
  "CL:0000651" = "CL:0002181"
)

.normalise_cl_release <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return(NA_character_)

  x <- trimws(as.character(x))
  match <- regexpr("\\d{4}-\\d{2}-\\d{2}", x, perl = TRUE)
  if (match[1L] < 0L) return(NA_character_)

  regmatches(x, match)
}

.cl_release_urls <- function(release = .CL_RELEASE) {
  release <- .normalise_cl_release(release)
  if (is.na(release)) {
    stop("`release` must contain a date in YYYY-MM-DD format.", call. = FALSE)
  }

  c(
    paste0(
      "https://purl.obolibrary.org/obo/cl/releases/",
      release,
      "/cl.obo"
    ),
    paste0(
      "https://github.com/obophenotype/cell-ontology/releases/download/v",
      release,
      "/cl.obo"
    ),
    paste0(
      "https://raw.githubusercontent.com/obophenotype/cell-ontology/v",
      release,
      "/cl.obo"
    )
  )
}

.cl_release_md5 <- function(release) {
  release <- .normalise_cl_release(release)
  if (!is.na(release) && identical(release, .CL_RELEASE)) {
    return(.CL_RELEASE_MD5)
  }
  NA_character_
}

.cl_release_from_url <- function(url) {
  release <- .normalise_cl_release(url)
  if (is.na(release)) return(NA_character_)
  if (url %in% .cl_release_urls(release)) release else NA_character_
}

.cl_release_from_header <- function(lines) {
  if (is.null(lines) || length(lines) == 0L) return(NA_character_)
  version_line <- grep("^data-version:", lines, value = TRUE)
  if (length(version_line) == 0L) return(NA_character_)
  .normalise_cl_release(version_line[1L])
}

.cl_ontology_release <- function(clData) {
  annotated <- attr(clData, "ontology_release", exact = TRUE)
  if (!is.null(annotated)) {
    release <- .normalise_cl_release(annotated)
    if (!is.na(release)) return(release)
  }
  .cl_release_from_header(attr(clData, "version", exact = TRUE))
}

.annotate_cl_release <- function(clData, source = NULL) {
  attr(clData, "ontology_release") <- .cl_ontology_release(clData)
  if (!is.null(source)) attr(clData, "ontology_source") <- source
  clData
}

.cl_cache_dir <- function() {
  cache_dir <- getOption("CellOnTools.cache_dir", NULL)
  if (is.null(cache_dir)) {
    cache_dir <- file.path(tools::R_user_dir("CellOnTools", "cache"), "ontology")
  }
  if (!is.character(cache_dir) || length(cache_dir) != 1L ||
      is.na(cache_dir) || !nzchar(cache_dir)) {
    stop(
      "Option `CellOnTools.cache_dir` must be a single non-empty path.",
      call. = FALSE
    )
  }
  path.expand(cache_dir)
}

.cl_cache_file <- function(release = .CL_RELEASE) {
  release <- .normalise_cl_release(release)
  if (is.na(release)) {
    stop("`release` must contain a date in YYYY-MM-DD format.", call. = FALSE)
  }
  file.path(.cl_cache_dir(), paste0("cl-", release, ".obo"))
}

.ols_cl_version <- function() {
  as.character(rols::olsVersion("cl"))
}

.assert_ols_cl_release <- function(expected = .CL_RELEASE) {
  expected <- .normalise_cl_release(expected)
  if (is.na(expected)) {
    stop("Internal error: invalid expected CL release.", call. = FALSE)
  }

  current <- tryCatch(
    .normalise_cl_release(.ols_cl_version()),
    error = function(e) {
      stop(
        "Could not verify the Cell Ontology release served by OLS: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  if (is.na(current)) {
    stop(
      "OLS returned an unrecognised Cell Ontology version; mapping was stopped ",
      "to avoid mixing ontology releases.",
      call. = FALSE
    )
  }
  if (!identical(current, expected)) {
    stop(
      "OLS currently serves Cell Ontology release ", current,
      ", but CellOnTools is pinned to ", expected,
      ". Mapping was stopped to avoid mixing ontology releases.",
      call. = FALSE
    )
  }

  invisible(current)
}
