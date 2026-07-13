# ============================================================================
# utils-validate.R
# Internal validation helpers for the CellOnTools package.
# All functions are unexported (prefixed with a dot).
# ============================================================================

# ----------------------------------------------------------------------------
# .validate_cldata
# Check that clData is a non-NULL ontology_index object.
# ----------------------------------------------------------------------------
.validate_cldata <- function(clData) {
  if (missing(clData) || is.null(clData)) {
    stop("`clData` must be provided (from CLload()).", call. = FALSE)
  }
  if (!inherits(clData, "ontology_index")) {
    stop("`clData` must be an ontology_index object.", call. = FALSE)
  }
  required_fields <- c("id", "name", "parents", "children", "ancestors")
  missing_fields <- setdiff(required_fields, names(clData))
  if (length(missing_fields) > 0L) {
    stop(
      "`clData` is missing required field(s): ",
      paste(missing_fields, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# ----------------------------------------------------------------------------
# .validate_integer_scalar
# Validate a finite whole-number scalar and return it as integer.
# ----------------------------------------------------------------------------
.validate_integer_scalar <- function(x,
                                     name,
                                     minimum = 0L,
                                     null_ok = FALSE) {
  if (null_ok && is.null(x)) return(NULL)

  valid <- is.numeric(x) && length(x) == 1L && !is.na(x) &&
    is.finite(x) && x == floor(x) && x >= minimum &&
    x <= .Machine$integer.max

  if (!valid) {
    requirement <- if (null_ok && minimum == 0L) {
      "NULL or a finite non-negative integer"
    } else if (!null_ok && minimum == 1L) {
      "a finite positive integer"
    } else if (null_ok) {
      paste0("NULL or a finite integer >= ", minimum)
    } else {
      paste0("a finite integer >= ", minimum)
    }
    stop("`", name, "` must be ", requirement, ".", call. = FALSE)
  }

  as.integer(x)
}

# ----------------------------------------------------------------------------
# .validate_logical_scalar
# Validate a non-missing TRUE/FALSE scalar.
# ----------------------------------------------------------------------------
.validate_logical_scalar <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", name, "` must be TRUE or FALSE.", call. = FALSE)
  }
  x
}

# ----------------------------------------------------------------------------
# .validate_ids
# Coerce, filter, and optionally warn about a character vector of CL IDs.
#
# Parameters
#   ids          : raw input (will be coerced to character)
#   clData       : ontology_index object (optional; needed for unknown-ID check)
#   unique_only  : if TRUE, silently deduplicate (for set-based functions)
#   allow_unknown: if FALSE, stop on IDs absent from clData
#   warn_invalid : if TRUE, emit a single aggregated warning for bad-format IDs
#
# Returns a cleaned character vector (preserves order; may contain duplicates
# unless unique_only = TRUE).
# ----------------------------------------------------------------------------
.validate_ids <- function(ids,
                          clData        = NULL,
                          unique_only   = FALSE,
                          allow_unknown = TRUE,
                          warn_invalid  = TRUE) {

  if (missing(ids) || is.null(ids) || length(ids) == 0L) {
    stop("`ids` must be a non-empty character vector of CL IDs.", call. = FALSE)
  }

  ids <- as.character(ids)
  ids <- ids[!is.na(ids) & nzchar(ids)]

  if (length(ids) == 0L) {
    stop("No valid IDs after removing NA and empty strings.", call. = FALSE)
  }

  if (unique_only) ids <- unique(ids)

  # ---- Format check ----
  is_bad_fmt <- !grepl("^CL:\\d+$", ids)
  bad_fmt <- unique(ids[is_bad_fmt])

  if (warn_invalid && length(bad_fmt) > 0L) {
    .warn_compact("Invalid CL ID format", bad_fmt)
  }

  if (!allow_unknown && length(bad_fmt) > 0L) {
    stop(
      "Invalid CL ID format: ",
      paste(head(bad_fmt, 3L), collapse = ", "),
      if (length(bad_fmt) > 3L) {
        paste0(" (and ", length(bad_fmt) - 3L, " more)")
      } else "",
      call. = FALSE
    )
  }

  # IMPORTANT: bad-format IDs are excluded from unknown-ID checks so that
  # users do not see both "invalid format" and "unknown ID" for the same value.
  ids_for_unknown_check <- ids[!is_bad_fmt]

  # ---- Existence check ----
  if (!is.null(clData)) {
    unknown <- unique(ids_for_unknown_check[!ids_for_unknown_check %in% clData$id])
    if (length(unknown) > 0L) {
      if (!allow_unknown) {
        stop(
          "Unknown CL IDs: ",
          paste(head(unknown, 3L), collapse = ", "),
          if (length(unknown) > 3L) paste0(" (and ", length(unknown) - 3L, " more)") else "",
          call. = FALSE
        )
      } else {
        .warn_compact("Unknown CL ID(s)", unknown)
      }
    }
  }

  ids
}

# ----------------------------------------------------------------------------
# .warn_compact
# Emit a single warning listing up to max_show values, with a count suffix.
# ----------------------------------------------------------------------------
.warn_compact <- function(prefix, values, max_show = 3L) {
  n <- length(values)
  shown <- head(values, max_show)
  suffix <- if (n > max_show) paste0(" (and ", n - max_show, " more)") else ""
  warning(prefix, ": ", paste(shown, collapse = ", "), suffix, call. = FALSE)
}
