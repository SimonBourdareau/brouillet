test_that("gradient_params accepts the pre-0.2 signature", {
  p <- gradient_params(colors = c("navy", "white", "firebrick"), midpoints = 0.5,
                       steepness_preset = "medium")
  expect_s3_class(p, "brouillet_gradient_params")
  expect_true(isTRUE(p$legacy))
  expect_equal(p$space, "sRGB")
  expect_false(p$rescale)
  expect_true(is.function(p$curve_fun))

  expect_identical(names(formals(gradient_params)),
                   c("colors", "midpoints", "sigma1", "sigma2", "inflexpoint1",
                     "inflexpoint2", "resolution", "steepness_preset",
                     "low.clip.color", "high.clip.color", "blend.clip",
                     "gradient_mode", "mapping_fun"))
})

test_that("legacy specifications work with every modern converter", {
  p <- gradient_params(colors = c("navy", "white", "firebrick"), midpoints = 0,
                       steepness_preset = "high")

  g <- buildGradient(p, limits = c(-2, 2))
  expect_equal(range(g$value), c(-2, 2))
  expect_true(all(isColor(g$color)))

  f <- asColorFun(p, limits = c(-2, 2))
  expect_colors_close(f(g$value), g$color)

  skip_if_not_installed("ggplot2")
  expect_s3_class(asScaleFillGradient(p, limits = c(-2, 2)), "Scale")
})

test_that("legacy midpoints are data units, resolved against the limits", {
  p <- gradient_params(colors = c("navy", "white", "firebrick"), midpoints = 25)
  g <- buildGradient(p, limits = c(0, 100))
  at25 <- grDevices::col2rgb(g$color[which.min(abs(g$value - 25))])
  expect_true(all(at25 > 220))

  expect_error(buildGradient(p, limits = c(0, 1)), "strictly inside the limits")
})

test_that("a preset overrides explicit sigmas, as it did before", {
  a <- buildGradient(gradient_params(c("red", "blue"), sigma1 = 80, sigma2 = 80))
  b <- buildGradient(gradient_params(c("red", "blue"), steepness_preset = "low"))
  expect_identical(a$color, b$color)

  c3 <- buildGradient(gradient_params(c("red", "blue"), sigma1 = 80, sigma2 = 80,
                                      steepness_preset = NULL))
  expect_false(identical(c3$color, b$color))
})

test_that("doubleSigmoid reproduces the legacy curve family", {
  f <- doubleSigmoid(200, 200, 0.3, 0.4)
  x <- seq(0, 1, length.out = 101)
  y <- f(x)
  expect_true(all(diff(y) >= 0))
  expect_true(all(y >= 0 & y <= 1))
  expect_gt(y[1], 0)
  expect_lt(y[length(y)], 1)

  staircase <- doubleSigmoid(5, 5, 0.25, 0.75)(x)
  expect_gt(sum(diff(staircase) > 0.02), 1)

  expect_error(doubleSigmoid(0, 200), "positive")
  expect_error(doubleSigmoid(200, 200, 0.8, 0.2), "less than")
})

test_that("curve_fun replaces the built-in curve per segment", {
  p <- gradientParams(c("navy", "white", "firebrick"), midpoints = 0.5,
                      curve_fun = doubleSigmoid(50, 50, 0.3, 0.7))
  expect_true(is.function(p$curve_fun))
  expect_true(all(isColor(buildGradient(p)$color)))

  expect_error(gradientParams(c("navy", "red"), curve_fun = "nope"), "must be a function")
  expect_error(gradientParams(c("navy", "red"), curve_fun = function(x) x * 3),
               "\\[0, 1\\]")
})

test_that("legacy semantics survive updateGradient and withColors", {
  p <- gradient_params(colors = c("navy", "white", "firebrick"), midpoints = 25)
  q <- updateGradient(p, resolution = 50)
  expect_true(isTRUE(q$legacy))
  expect_equal(q$midpoints, 25)

  tmpl <- gradient_params(colors = NULL, steepness_preset = "high")
  r <- withColors(tmpl, c("black", "white"))
  expect_true(isTRUE(r$legacy))
  expect_true(all(isColor(buildGradient(r)$color)))
})
