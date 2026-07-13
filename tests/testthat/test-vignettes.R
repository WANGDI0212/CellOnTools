find_vignette_source <- function(filename) {
  candidates <- c(
    file.path("vignettes", filename),
    testthat::test_path("..", "..", "vignettes", filename),
    system.file("doc", filename, package = "CellOnTools")
  )
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (!length(candidates)) return(NA_character_)
  normalizePath(candidates[[1L]], winslash = "/", mustWork = TRUE)
}

test_that("vignette purl output excludes optional and network-heavy code", {
  skip_if_not_installed("knitr")

  forbidden <- paste0(
    "\\b(CLdownload|CLload|CLmap|CLmapInteractive|LoadData|Seurat|",
    "clusterProfiler|ontologySimilarity)\\b"
  )

  for (filename in c("CellOnTools.Rmd", "pbmc3k-workflow.Rmd")) {
    input <- find_vignette_source(filename)
    expect_false(is.na(input), info = filename)
    if (is.na(input)) next

    output <- tempfile(fileext = ".R")
    expect_no_error(
      knitr::purl(
        input,
        output = output,
        documentation = 0,
        quiet = TRUE
      )
    )
    code <- readLines(output, warn = FALSE)
    expect_length(grep(forbidden, code, perl = TRUE), 0L)
  }
})
