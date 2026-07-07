#' Download Cell Ontology OBO File
#'
#' @description
#' Downloads the Cell Ontology OBO file from OBO Foundry (or a custom URL) and
#' performs a lightweight header validation to confirm the file is a valid OBO
#' document. For the default Cell Ontology URL, the function retries through a
#' raw GitHub mirror and, on Windows, an external curl fallback to work around
#' common SSL/revocation issues.
#'
#' @param dest_file Destination file path (default: \code{"cl.obo"} in the
#'   current working directory).
#' @param url URL to download from (default: latest release from OBO Foundry).
#' @param overwrite Logical; if \code{FALSE} (default), stop if the destination
#'   file already exists.
#'
#' @return Invisibly returns the normalised path to the downloaded file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Download to current directory
#' CLdownload()
#'
#' # Download to a specific location
#' CLdownload(dest_file = "data/cl.obo")
#'
#' # Download a specific versioned release
#' CLdownload(url = "https://purl.obolibrary.org/obo/cl/releases/2023-08-24/cl.obo")
#'
#' # Load the downloaded file
#' clData <- CLload(local_obo = "cl.obo", prefer_local = TRUE)
#' }
CLdownload <- function(dest_file = "cl.obo",
                       url       = "https://purl.obolibrary.org/obo/cl.obo",
                       overwrite = FALSE) {

  # ---- Validate arguments ----
  if (!is.character(dest_file) || length(dest_file) != 1L || !nzchar(dest_file)) {
    stop("`dest_file` must be a single non-empty character string.", call. = FALSE)
  }
  if (!is.character(url) || length(url) != 1L || !nzchar(url)) {
    stop("`url` must be a single non-empty character string.", call. = FALSE)
  }
  if (!grepl("^https?://", url)) {
    stop("`url` must start with http:// or https://", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("`overwrite` must be TRUE or FALSE.", call. = FALSE)
  }

  if (file.exists(dest_file) && !overwrite) {
    stop("File already exists: ", dest_file,
         "\nSet overwrite = TRUE to replace it.", call. = FALSE)
  }

  # ---- Create destination directory if needed ----
  dest_dir <- dirname(dest_file)
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
    message("Created directory: ", dest_dir)
  }

  # ---- Download ----
  message("Downloading Cell Ontology OBO file...")
  message("  From: ", url)
  message("  To:   ", dest_file)

  old_timeout <- getOption("timeout")
  if (is.numeric(old_timeout) && length(old_timeout) == 1L && old_timeout < 300) {
    options(timeout = 300)
    on.exit(options(timeout = old_timeout), add = TRUE)
  }

  default_url <- "https://purl.obolibrary.org/obo/cl.obo"
  release_url <- "https://github.com/obophenotype/cell-ontology/releases/latest/download/cl.obo"
  raw_url <- "https://raw.githubusercontent.com/obophenotype/cell-ontology/master/cl.obo"
  download_urls <- url
  if (identical(url, default_url) || identical(url, release_url)) {
    download_urls <- unique(c(download_urls, raw_url))
  }

  method_candidates <- getOption("download.file.method", "auto")
  if (is.null(method_candidates) || !nzchar(method_candidates)) {
    method_candidates <- "auto"
  }
  if (isTRUE(capabilities("libcurl"))) {
    method_candidates <- c(method_candidates, "libcurl")
  }
  if (nzchar(Sys.which("curl"))) {
    method_candidates <- c(method_candidates, "curl")
  }
  method_candidates <- unique(method_candidates)

  tmp_file <- tempfile(pattern = paste0(basename(dest_file), "."),
                       tmpdir = dest_dir, fileext = ".download")
  on.exit(unlink(tmp_file), add = TRUE)

  errors <- character()
  status <- 1L
  downloaded_url <- NA_character_
  downloaded_method <- NA_character_

  for (attempt_url in download_urls) {
    for (method in method_candidates) {
      if (file.exists(tmp_file)) unlink(tmp_file)

      extra <- NULL
      if (identical(method, "curl")) {
        extra <- "-L"
        if (.Platform$OS.type == "windows") {
          extra <- paste(extra, "--ssl-no-revoke")
        }
      }

      message("  Trying: ", attempt_url, " [method = ", method, "]")
      args <- list(url = attempt_url, destfile = tmp_file, method = method,
                   quiet = FALSE, mode = "wb")
      if (!is.null(extra)) {
        args$extra <- extra
      }

      status <- tryCatch(
        do.call(utils::download.file, args),
        error = function(e) e
      )

      if (inherits(status, "error")) {
        errors <- c(errors, paste0(attempt_url, " [", method, "]: ",
                                   status$message))
        next
      }

      if (!identical(status, 0L) && !identical(status, 0)) {
        errors <- c(errors, paste0(attempt_url, " [", method,
                                   "]: download.file() returned status ",
                                   status))
        next
      }

      if (!file.exists(tmp_file) || is.na(file.size(tmp_file)) ||
          file.size(tmp_file) == 0L) {
        errors <- c(errors, paste0(attempt_url, " [", method,
                                   "]: downloaded file is missing or empty"))
        next
      }

      downloaded_url <- attempt_url
      downloaded_method <- method
      break
    }

    if (!is.na(downloaded_url)) break
  }

  if (is.na(downloaded_url)) {
    stop("Download failed after trying available methods.\n",
         paste0("  - ", errors, collapse = "\n"),
         "\nTip: if this is an SSL issue on Windows, try downloading the file ",
         "manually with PowerShell or curl and then use CLload(local_obo = ...).",
         call. = FALSE)
  }

  # ---- Verify file ----
  file_size <- file.size(tmp_file)
  if (file_size == 0L) {
    stop("Downloaded file is empty: ", tmp_file, call. = FALSE)
  }

  # ---- Lightweight OBO header validation ----
  header <- tryCatch(readLines(tmp_file, n = 30L, warn = FALSE), error = function(e) NULL)
  if (!is.null(header)) {
    has_format  <- any(grepl("^format-version:", header))
    has_term    <- any(grepl("^\\[Term\\]$", header))
    if (!has_format && !has_term) {
      warning("Downloaded file does not appear to be a valid OBO document. ",
              "Check the URL and try again.", call. = FALSE)
    }
  }

  copied <- file.copy(tmp_file, dest_file, overwrite = TRUE)
  if (!isTRUE(copied)) {
    stop("Downloaded file could not be written to destination: ", dest_file,
         call. = FALSE)
  }

  message("Successfully downloaded Cell Ontology OBO file")
  message("  Source:   ", downloaded_url)
  message("  Method:   ", downloaded_method)
  message("  Size:     ", format(file_size, big.mark = ","), " bytes")
  message("  Location: ", normalizePath(dest_file))

  invisible(normalizePath(dest_file))
}
