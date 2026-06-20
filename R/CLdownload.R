#' Download Cell Ontology OBO File
#'
#' @description
#' Downloads the Cell Ontology OBO file from OBO Foundry (or a custom URL) and
#' performs a lightweight header validation to confirm the file is a valid OBO
#' document.
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

  status <- tryCatch(
    download.file(url = url, destfile = dest_file, method = "auto",
                  quiet = FALSE, mode = "wb"),
    error = function(e) {
      stop("Download failed.\n  URL: ", url, "\n  Error: ", e$message, call. = FALSE)
    }
  )

  if (!identical(status, 0L) && !identical(status, 0)) {
    stop("download.file() returned non-zero status (", status,
         "). The file may be incomplete.", call. = FALSE)
  }

  # ---- Verify file ----
  if (!file.exists(dest_file)) {
    stop("Download appeared to succeed but file not found: ", dest_file, call. = FALSE)
  }
  file_size <- file.size(dest_file)
  if (file_size == 0L) {
    stop("Downloaded file is empty: ", dest_file, call. = FALSE)
  }

  # ---- Lightweight OBO header validation ----
  header <- tryCatch(readLines(dest_file, n = 30L, warn = FALSE), error = function(e) NULL)
  if (!is.null(header)) {
    has_format  <- any(grepl("^format-version:", header))
    has_term    <- any(grepl("^\\[Term\\]$", header))
    if (!has_format && !has_term) {
      warning("Downloaded file does not appear to be a valid OBO document. ",
              "Check the URL and try again.", call. = FALSE)
    }
  }

  message("Successfully downloaded Cell Ontology OBO file")
  message("  Size:     ", format(file_size, big.mark = ","), " bytes")
  message("  Location: ", normalizePath(dest_file))

  invisible(normalizePath(dest_file))
}
