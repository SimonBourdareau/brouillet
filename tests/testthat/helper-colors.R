# Colours are quantised to 8 bits, so mathematically identical routes through
# the interpolation can differ by one code value. Compare with a tolerance.
expect_colors_close <- function(actual, expected, tol = 1) {
  a <- grDevices::col2rgb(actual)
  b <- grDevices::col2rgb(expected)
  testthat::expect_equal(dim(a), dim(b))
  testthat::expect_lte(max(abs(a - b)), tol)
}
