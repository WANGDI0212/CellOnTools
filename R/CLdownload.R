#' Download Cell Ontology OBO File
#'
#' @description
#' Downloads the Cell Ontology OBO file from OBO Foundry (or a custom URL) and
#' validates the downloaded content before replacing the destination file. The
#' default is the fixed \code{2026-06-08} release. Its OBO version header and MD5
#' checksum are verified, and all fallback URLs point to the same tagged
#' release rather than to a moving latest/master branch.
#'
#' @details
#' Each network attempt is limited to 60 seconds and the complete fallback
#' sequence to 180 seconds by default. Adjust these limits with options
#' `CellOnTools.download_timeout` and `CellOnTools.download_total_timeout`.
#' Both values are in seconds.
#'
#' @param dest_file Destination file path (default: \code{"cl.obo"} in the
#'   current working directory).
#' @param url URL to download from (default: the versioned OBO Foundry URL for
#'   Cell Ontology release \code{2026-06-08}). Custom URLs are structurally
#'   validated as Cell Ontology OBO files but are not required to match the
#'   package's pinned release.
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
#' # Download a different versioned release explicitly
#' CLdownload(url = "https://purl.obolibrary.org/obo/cl/releases/2025-12-16/cl.obo")
#'
#' # Load the downloaded file
#' clData <- CLload(local_obo = "cl.obo", prefer_local = TRUE)
#' }
CLdownload <- function(dest_file = "cl.obo",
                       url       = "https://purl.obolibrary.org/obo/cl/releases/2026-06-08/cl.obo",
                       overwrite = FALSE) {

  # ---- Validate arguments ----
  if (!is.character(dest_file) || length(dest_file) != 1L ||
      is.na(dest_file) || !nzchar(trimws(dest_file))) {
    stop("`dest_file` must be a single non-empty character string.", call. = FALSE)
  }
  if (!is.character(url) || length(url) != 1L ||
      is.na(url) || !nzchar(trimws(url))) {
    stop("`url` must be a single non-empty character string.", call. = FALSE)
  }
  if (!grepl("^https?://", url)) {
    stop("`url` must start with http:// or https://", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("`overwrite` must be TRUE or FALSE.", call. = FALSE)
  }

  # ---- Create destination directory if needed ----
  dest_dir <- dirname(dest_file)
  if (!dir.exists(dest_dir)) {
    created <- dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
    if (!isTRUE(created) && !dir.exists(dest_dir)) {
      stop("Could not create destination directory: ", dest_dir,
           call. = FALSE)
    }
    message("Created directory: ", dest_dir)
  }

  # Serialise writers for a destination. The lock is acquired before checking
  # for an existing file so concurrent callers cannot race between the check
  # and the final commit.
  lock_dir <- .acquire_cl_file_lock(dest_file)
  on.exit(.release_cl_file_lock(lock_dir), add = TRUE)

  .download_cl_obo_locked(
    dest_file = dest_file,
    url = url,
    overwrite = overwrite
  )
}

.validate_cl_download_timeout <- function(value, option_name) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || value < 1) {
    stop("Option `", option_name, "` must be a finite number >= 1.",
         call. = FALSE)
  }
  as.numeric(value)
}

# Download and commit an OBO file while the caller holds the destination lock.
# Keeping this work in a private helper lets CLload() re-check a cache after it
# acquires the lock, avoiding a second download when another process won the
# race and populated the cache first.
.download_cl_obo_locked <- function(dest_file, url, overwrite) {
  if (file.exists(dest_file) && !overwrite) {
    stop("File already exists: ", dest_file,
         "\nSet overwrite = TRUE to replace it.", call. = FALSE)
  }

  message("Downloading Cell Ontology OBO file...")
  message("  From: ", url)
  message("  To:   ", dest_file)

  per_attempt_timeout <- .validate_cl_download_timeout(
    getOption("CellOnTools.download_timeout", 60),
    "CellOnTools.download_timeout"
  )
  total_timeout <- .validate_cl_download_timeout(
    getOption("CellOnTools.download_total_timeout", 180),
    "CellOnTools.download_total_timeout"
  )
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)

  download_urls <- url
  expected_release <- NULL
  expected_md5 <- NULL
  url_release <- .cl_release_from_url(url)
  if (!is.na(url_release)) {
    download_urls <- unique(c(url, .cl_release_urls(url_release)))
    expected_release <- url_release
    expected_md5 <- .cl_release_md5(url_release)
  }

  configured_method <- getOption("download.file.method", "auto")
  if (!is.character(configured_method) || length(configured_method) != 1L ||
      is.na(configured_method) || !nzchar(configured_method)) {
    configured_method <- "auto"
  }
  method_candidates <- configured_method
  # On current R builds, auto already selects libcurl. Retrying explicit
  # libcurl would repeat the same failure and multiply the wait time.
  if (!identical(configured_method, "auto") &&
      isTRUE(capabilities("libcurl"))) {
    method_candidates <- c(method_candidates, "libcurl")
  }
  if (nzchar(Sys.which("curl"))) {
    method_candidates <- c(method_candidates, "curl")
  }
  method_candidates <- unique(method_candidates)

  dest_dir <- dirname(dest_file)
  tmp_file <- tempfile(pattern = paste0(basename(dest_file), "."),
                       tmpdir = dest_dir, fileext = ".download")
  on.exit(unlink(tmp_file), add = TRUE)

  errors <- character()
  downloaded_url <- NA_character_
  downloaded_method <- NA_character_
  total_started <- Sys.time()
  total_timed_out <- FALSE

  for (attempt_url in download_urls) {
    for (method in method_candidates) {
      elapsed <- as.numeric(difftime(
        Sys.time(), total_started, units = "secs"
      ))
      remaining <- total_timeout - elapsed
      if (!is.finite(remaining) || remaining < 1) {
        total_timed_out <- TRUE
        break
      }

      attempt_timeout <- max(1, min(per_attempt_timeout, remaining))
      options(timeout = as.integer(ceiling(attempt_timeout)))
      if (file.exists(tmp_file)) unlink(tmp_file)

      extra <- NULL
      if (identical(method, "curl")) {
        extra <- paste("-L --max-time", as.integer(ceiling(attempt_timeout)))
        if (.Platform$OS.type == "windows") {
          extra <- paste(extra, "--ssl-no-revoke")
        }
      }

      message(
        "  Trying: ", attempt_url, " [method = ", method,
        ", timeout = ", as.integer(ceiling(attempt_timeout)), "s]"
      )
      args <- list(url = attempt_url, destfile = tmp_file, method = method,
                   quiet = FALSE, mode = "wb")
      if (!is.null(extra)) {
        args$extra <- extra
      }

      status <- tryCatch(
        .download_obo_file(args),
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

      validation_error <- tryCatch(
        {
          .validate_cl_obo_file(
            tmp_file,
            expected_release = expected_release,
            expected_md5 = expected_md5
          )
          NULL
        },
        error = function(e) conditionMessage(e)
      )
      if (!is.null(validation_error)) {
        errors <- c(errors, paste0(attempt_url, " [", method,
                                   "]: ", validation_error))
        next
      }

      downloaded_url <- attempt_url
      downloaded_method <- method
      break
    }

    if (!is.na(downloaded_url) || total_timed_out) break
  }

  if (is.na(downloaded_url)) {
    if (total_timed_out) {
      errors <- c(
        errors,
        paste0("total download time budget of ", total_timeout, " seconds exceeded")
      )
    }
    stop("Download failed after trying available methods.\n",
         paste0("  - ", unique(errors), collapse = "\n"),
         "\nTip: if this is an SSL issue on Windows, try downloading the file ",
         "manually with PowerShell or curl and then use CLload(local_obo = ...).",
         call. = FALSE)
  }

  # The temporary file has passed structural OBO validation. Only now may it
  # replace the destination, so an invalid response cannot destroy an existing
  # file even when overwrite = TRUE.
  file_size <- file.size(tmp_file)
  .commit_cl_download(tmp_file, dest_file, overwrite = overwrite)

  message("Successfully downloaded Cell Ontology OBO file")
  message("  Source:   ", downloaded_url)
  message("  Method:   ", downloaded_method)
  message("  Size:     ", format(file_size, big.mark = ","), " bytes")
  message("  Location: ", normalizePath(dest_file))

  invisible(normalizePath(dest_file))
}

# Acquire a cross-process lock represented by an atomically created directory.
# Stale locks are removed so a terminated R session cannot block the cache
# indefinitely. This deliberately uses base R to avoid a mandatory dependency
# for the default ontology-loading path.
.acquire_cl_file_lock <- function(dest_file,
                                  timeout = 300,
                                  poll = 0.1,
                                  stale_after = 3600) {
  lock_dir <- paste0(dest_file, ".lock")
  started <- Sys.time()

  repeat {
    if (isTRUE(dir.create(lock_dir, showWarnings = FALSE))) {
      owner <- c(
        paste0("pid=", Sys.getpid()),
        paste0("created=", format(Sys.time(), tz = "UTC", usetz = TRUE))
      )
      try(writeLines(owner, file.path(lock_dir, "owner")), silent = TRUE)
      return(lock_dir)
    }

    info <- suppressWarnings(file.info(lock_dir))
    lock_age <- if (nrow(info) == 1L && !is.na(info$mtime)) {
      as.numeric(difftime(Sys.time(), info$mtime, units = "secs"))
    } else {
      NA_real_
    }
    if (is.finite(lock_age) && lock_age > stale_after) {
      unlink(lock_dir, recursive = TRUE, force = TRUE)
      next
    }

    waited <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    if (waited >= timeout) {
      stop(
        "Timed out waiting for another process to finish writing: ",
        dest_file,
        call. = FALSE
      )
    }
    Sys.sleep(poll)
  }
}

.release_cl_file_lock <- function(lock_dir) {
  if (is.character(lock_dir) && length(lock_dir) == 1L &&
      !is.na(lock_dir) && dir.exists(lock_dir)) {
    unlink(lock_dir, recursive = TRUE, force = TRUE)
  }
  invisible(NULL)
}

# Install a validated download using same-directory renames. When replacing an
# existing file, keep a temporary backup until the new file is in place so a
# failed commit can restore the previous cache.
.commit_cl_download <- function(tmp_file, dest_file, overwrite = FALSE) {
  if (!file.exists(tmp_file)) {
    stop("Validated temporary download is missing: ", tmp_file, call. = FALSE)
  }

  if (!file.exists(dest_file)) {
    if (!isTRUE(file.rename(tmp_file, dest_file))) {
      stop("Downloaded file could not be committed to destination: ",
           dest_file, call. = FALSE)
    }
    return(invisible(dest_file))
  }

  if (!overwrite) {
    stop("File already exists: ", dest_file, call. = FALSE)
  }

  backup <- tempfile(
    pattern = paste0(".", basename(dest_file), ".backup-"),
    tmpdir = dirname(dest_file)
  )
  if (!isTRUE(file.rename(dest_file, backup))) {
    stop("Existing destination could not be moved aside safely: ", dest_file,
         call. = FALSE)
  }

  committed <- FALSE
  on.exit({
    if (!committed && file.exists(backup) && !file.exists(dest_file)) {
      file.rename(backup, dest_file)
    }
  }, add = TRUE)

  if (!isTRUE(file.rename(tmp_file, dest_file))) {
    restored <- isTRUE(file.rename(backup, dest_file))
    stop(
      "Downloaded file could not be committed to destination: ", dest_file,
      if (!restored) paste0(". Previous file remains at ", backup) else "",
      call. = FALSE
    )
  }

  committed <- TRUE
  if (file.exists(backup) && unlink(backup, force = TRUE) != 0L) {
    warning("Could not remove download backup file: ", backup, call. = FALSE)
  }
  invisible(dest_file)
}

.validate_cl_obo_file <- function(path,
                                  expected_release = NULL,
                                  expected_md5 = NULL) {
  if (!file.exists(path) || is.na(file.size(path)) || file.size(path) == 0L) {
    stop("Downloaded file is missing or empty.", call. = FALSE)
  }

  con <- file(path, open = "rt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)

  has_format <- FALSE
  has_cl_ontology <- FALSE
  has_term <- FALSE
  has_cl_term <- FALSE
  looks_like_html <- FALSE
  in_term_stanza <- FALSE
  version_lines <- character()

  repeat {
    lines <- tryCatch(
      suppressWarnings(readLines(con, n = 1000L, warn = FALSE)),
      error = function(e) {
        stop("Downloaded file could not be read as text: ",
             conditionMessage(e), call. = FALSE)
      }
    )
    if (length(lines) == 0L) break

    lines <- trimws(lines)
    lines[1L] <- sub("^\ufeff", "", lines[1L])

    looks_like_html <- looks_like_html || any(grepl(
      "^(<!doctype\\s+html|<html(?:\\s|>))",
      lines,
      ignore.case = TRUE,
      perl = TRUE
    ))
    has_format <- has_format || any(grepl(
      "^format-version:\\s*\\S+", lines, perl = TRUE
    ))
    has_cl_ontology <- has_cl_ontology || any(grepl(
      "^ontology:\\s*cl\\s*$", lines, ignore.case = TRUE, perl = TRUE
    ))
    version_lines <- c(version_lines, grep("^data-version:", lines, value = TRUE))

    for (line in lines) {
      if (identical(line, "[Term]")) {
        has_term <- TRUE
        in_term_stanza <- TRUE
        next
      }
      if (grepl("^\\[[^]]+\\]$", line)) {
        in_term_stanza <- FALSE
        next
      }
      if (in_term_stanza && grepl("^id:\\s*CL:\\d+\\s*$", line, perl = TRUE)) {
        has_cl_term <- TRUE
      }
    }

  }

  problems <- character()
  if (looks_like_html) problems <- c(problems, "response appears to be HTML")
  if (!has_format) problems <- c(problems, "missing format-version header")
  if (!has_cl_ontology) problems <- c(problems, "missing ontology: cl header")
  if (!has_term) problems <- c(problems, "missing [Term] stanza")
  if (!has_cl_term) problems <- c(problems, "missing CL term ID")

  if (!is.null(expected_release)) {
    actual_release <- .cl_release_from_header(version_lines)
    expected_release <- .normalise_cl_release(expected_release)
    if (is.na(actual_release)) {
      problems <- c(problems, "missing or unrecognised data-version release")
    } else if (!identical(actual_release, expected_release)) {
      problems <- c(
        problems,
        paste0(
          "release mismatch (expected ", expected_release,
          ", found ", actual_release, ")"
        )
      )
    }
  }

  if (!is.null(expected_md5) && !is.na(expected_md5) && nzchar(expected_md5)) {
    actual_md5 <- tolower(unname(tools::md5sum(path)))
    if (is.na(actual_md5) || !identical(actual_md5, tolower(expected_md5))) {
      problems <- c(
        problems,
        paste0(
          "checksum mismatch (expected ", tolower(expected_md5),
          ", found ", actual_md5, ")"
        )
      )
    }
  }

  if (length(problems) == 0L) return(invisible(TRUE))

  stop(
    "Downloaded content is not a valid Cell Ontology OBO document (",
    paste(problems, collapse = "; "), ").",
    call. = FALSE
  )
}

.download_obo_file <- function(args) {
  do.call(utils::download.file, args)
}
