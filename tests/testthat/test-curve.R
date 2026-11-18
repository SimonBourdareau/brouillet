x <- seq(0, 1, length.out = 2001)

quantile_at <- function(y, p) x[which(y >= p)[1]]

test_that("contrastCurve is monotone and hits both endpoints", {
  for (kw in list(list(), list(width = 0.1), list(width = 0.9), list(width = 2),
                  list(center = 0.2), list(asymmetry = 0.8),
                  list(asymmetry = -0.8))) {
    y <- do.call(contrastCurve, c(list(x), kw))
    expect_true(all(diff(y) >= -1e-12), label = paste(names(kw), collapse = ","))
    expect_true(all(y >= 0 & y <= 1))
    expect_equal(y[1], 0, tolerance = 1e-9)
    expect_equal(y[length(y)], 1, tolerance = 1e-9)
  }
})

test_that("center is the 50% point", {
  for (c0 in c(0.2, 0.5, 0.8)) {
    y <- contrastCurve(x, center = c0, width = 0.3)
    expect_lt(abs(quantile_at(y, 0.5) - c0), 0.01)
  }
})

test_that("width is the 5-95% span", {
  # Rescaling clips the tails, so wide curves come out slightly narrower than
  # requested; the mapping is exact for transitions that fit inside the range.
  for (w in c(0.1, 0.3)) {
    y <- contrastCurve(x, center = 0.5, width = w)
    expect_lt(abs(quantile_at(y, 0.95) - quantile_at(y, 0.05) - w), 0.01)
  }
  y <- contrastCurve(x, center = 0.5, width = 0.6)
  expect_lt(abs(quantile_at(y, 0.95) - quantile_at(y, 0.05) - 0.6), 0.05)
})

test_that("asymmetry moves softness from the low side to the high side", {
  y <- contrastCurve(x, center = 0.5, width = 0.4, asymmetry = 0.8)
  below <- 0.5 - quantile_at(y, 0.05)
  above <- quantile_at(y, 0.95) - 0.5
  expect_lt(below, above)

  y2 <- contrastCurve(x, center = 0.5, width = 0.4, asymmetry = -0.8)
  expect_gt(0.5 - quantile_at(y2, 0.05), quantile_at(y2, 0.95) - 0.5)
})

test_that("rescale = FALSE leaves the ends short of 0 and 1", {
  y <- contrastCurve(x, width = 0.4, rescale = FALSE)
  expect_gt(y[1], 0)
  expect_lt(y[length(y)], 1)
})

test_that("wide curves approach the identity", {
  y <- contrastCurve(x, width = 1.2)
  expect_lt(max(abs(y - x)), 0.1)
})

test_that("contrastCurve validates its arguments", {
  expect_error(contrastCurve("a"), "must be numeric")
  expect_error(contrastCurve(x, center = 2), "between 0 and 1")
  expect_error(contrastCurve(x, width = 0), "positive")
  expect_error(contrastCurve(x, asymmetry = 3), "between -1 and 1")
  expect_error(contrastCurve(x, rescale = NA), "logical")
})

test_that("linear curve is the identity by default and clamps when narrower", {
  y <- brouillet:::.linear_curve(x)
  expect_equal(y, x, tolerance = 1e-9)

  y2 <- brouillet:::.linear_curve(x, center = 0.5, width = 0.5)
  expect_equal(y2[x <= 0.25], rep(0, sum(x <= 0.25)))
  expect_equal(y2[x >= 0.75], rep(1, sum(x >= 0.75)))
})

