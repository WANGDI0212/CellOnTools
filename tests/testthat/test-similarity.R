test_that("CLsimilarity computes Resnik and Lin scores", {
  testthat::skip_if_not_installed("ontologyIndex")
  testthat::skip_if_not_installed("ontologySimilarity")
  cl <- test_cl_dag()
  ic <- ontologySimilarity::descendants_IC(cl)

  resnik <- CLsimilarity("CL:0000300", "CL:0000400", cl,
                         method = "resnik", information_content = ic)
  lin <- CLsimilarity("CL:0000300", "CL:0000400", cl,
                      method = "lin", information_content = ic)

  expect_length(resnik, 1L)
  expect_true(is.finite(resnik) && resnik >= 0)
  # Lin similarity is normalised to [0, 1].
  expect_true(lin >= 0 && lin <= 1)

  # A term is maximally similar to itself under Lin.
  self_lin <- CLsimilarity("CL:0000400", "CL:0000400", cl,
                           method = "lin", information_content = ic)
  expect_equal(self_lin, 1, tolerance = 1e-8)
})

test_that("CLsimilarity validates its term arguments", {
  testthat::skip_if_not_installed("ontologySimilarity")
  cl <- test_cl_dag()

  expect_error(CLsimilarity("bad_id", "CL:0000400", cl), "invalid CL ID format")
  expect_error(CLsimilarity("CL:0000300", "CL:9999999", cl), "Unknown CL ID")
})

test_that("CLsimilarityMatrix preserves input order and duplicates", {
  testthat::skip_if_not_installed("ontologyIndex")
  testthat::skip_if_not_installed("ontologySimilarity")
  cl <- test_cl_dag()
  ic <- ontologySimilarity::descendants_IC(cl)

  ids <- c("CL:0000300", "CL:0000400", "CL:0000300")  # duplicate row term
  m <- CLsimilarityMatrix(ids, clData = cl, information_content = ic,
                          verbose = FALSE)

  expect_equal(dim(m), c(3L, 3L))
  # Duplicated query yields identical rows.
  expect_equal(m[1, ], m[3, ])
  # Symmetric matrix when ids1 == ids2.
  expect_equal(m, t(m), tolerance = 1e-8)

  # Cross comparison has the requested shape.
  cross <- CLsimilarityMatrix(
    ids1 = c("CL:0000300", "CL:0000400"),
    ids2 = c("CL:0000600"),
    clData = cl, information_content = ic, verbose = FALSE
  )
  expect_equal(dim(cross), c(2L, 1L))
})
