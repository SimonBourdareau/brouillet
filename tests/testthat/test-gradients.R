expect_gradient_df <- function(g, low = 0, high = 1) {
  expect_s3_class(g, "data.frame")
  expect_named(g, c("value", "color"))
  expect_true(all(isColor(g$color)))
  expect_false(is.unsorted(g$value))
  expect_equal(min(g$value), low)
  expect_equal(max(g$value), high)
}

test_that("sigmoidGradient2 builds a valid gradient", {
  g <- sigmoidGradient2("navy", "firebrick")
  expect_gradient_df(g)
  expect_equal(nrow(g), 101)
  expect_equal(attr(g, "gradient_type"), "sigmoidGradient2")
  # Ends reach the requested colours exactly (rescale = TRUE by default).
  expect_equal(toupper(g$color[1]), "#000080")        # navy
  expect_equal(toupper(g$color[nrow(g)]), "#B22222")  # firebrick
})

test_that("width controls how sharp the transition is", {
  sharp <- sigmoidGradient2("black", "white", width = 0.05, resolution = 200)
  soft <- sigmoidGradient2("black", "white", width = 1, resolution = 200)

  lum <- function(g) as.numeric(grDevices::col2rgb(g$color)[1, ])
  # A sharp curve spends most of the range near one end or the other.
  expect_gt(mean(lum(sharp) < 20 | lum(sharp) > 235),
            mean(lum(soft) < 20 | lum(soft) > 235))
})

test_that("center moves the transition along the range", {
  lum <- function(g) as.numeric(grDevices::col2rgb(g$color)[1, ])
  low <- sigmoidGradient2("black", "white", center = 0.2, width = 0.2)
  high <- sigmoidGradient2("black", "white", center = 0.8, width = 0.2)
  # Position where the ramp crosses mid-grey.
  cross <- function(g) g$value[which(lum(g) >= 128)[1]]
  expect_lt(cross(low), 0.35)
  expect_gt(cross(high), 0.65)
})

test_that("sigmoidGradient2 validates its arguments", {
  expect_error(sigmoidGradient2("navy", "nope"), "not a single valid colour")
  expect_error(sigmoidGradient2("navy", "red", center = 2), "between 0 and 1")
  expect_error(sigmoidGradient2("navy", "red", width = -1), "positive")
  expect_error(sigmoidGradient2("navy", "red", asymmetry = 5), "between -1 and 1")
  expect_error(sigmoidGradient2("navy", "red", low.value = 1, high.value = 0),
               "less than")
  expect_error(sigmoidGradient2("navy", "red", space = "hsv"), "Lab")
  expect_warning(sigmoidGradient2("navy", "red", resolution = 1), "resolution")
})

test_that("segments run in colour order (0.1.0 reversed them)", {
  g <- sigmoidGradientn(c("black", "red", "white"), 0.5, resolution = 100)
  first <- grDevices::col2rgb(g$color[1])
  last <- grDevices::col2rgb(g$color[nrow(g)])
  expect_true(all(first < 20))            # black at the low end
  expect_true(all(last > 235))            # white at the high end
  # Red near the interior stop, not at an end.
  mid <- grDevices::col2rgb(g$color[which.min(abs(g$value - 0.5))])
  expect_gt(mid[1], mid[2] + 50)
})

test_that("sigmoidGradient3 is symmetric about its midpoint by default", {
  g <- sigmoidGradient3("black", "black", "white", resolution = 100)
  lum <- as.numeric(grDevices::col2rgb(g$color)[1, ])
  expect_lt(max(abs(lum - rev(lum))), 3)
  expect_true(any(abs(g$value - 0.5) < 1e-8))
  expect_error(sigmoidGradient3("navy", "red", "white", midpoint.value = 2),
               "low.value < midpoint.value < high.value")
})

test_that("midpoints default to evenly spaced stops", {
  g1 <- sigmoidGradientn(c("navy", "white", "firebrick"))
  g2 <- sigmoidGradientn(c("navy", "white", "firebrick"), midpoints = 0.5)
  expect_equal(g1$color, g2$color)
  expect_error(sigmoidGradientn(c("navy", "white", "firebrick"), c(0.3, 0.6)),
               "length\\(colors\\) - 2")
  expect_error(sigmoidGradientn(c("navy", "white", "firebrick", "black"),
                                 c(0.6, 0.3)), "increasing")
})

test_that("gradients respect non-default limits", {
  g <- sigmoidGradientn(c("navy", "white", "firebrick", "black"), c(0.25, 0.75),
                         low.value = -10, high.value = 10)
  expect_gradient_df(g, low = -10, high = 10)

  g2 <- linearGradientn(c("navy", "white", "firebrick"), 0.5,
                         low.value = -5, high.value = 5)
  expect_gradient_df(g2, low = -5, high = 5)
})

test_that("linearGradient2 is a plain ramp by default", {
  g <- linearGradient2("black", "white", resolution = 10)
  expect_gradient_df(g)
  expect_equal(toupper(g$color[1]), "#000000")
  expect_equal(toupper(g$color[nrow(g)]), "#FFFFFF")

  # width < 1 leaves flat regions at both ends.
  gf <- linearGradient2("black", "white", center = 0.5, width = 0.5,
                         resolution = 10)
  expect_equal(toupper(gf$color[1:3]), rep("#000000", 3))
  expect_equal(toupper(gf$color[9:11]), rep("#FFFFFF", 3))
})

test_that("customGradientFun requires a monotonic mapping in [0, 1]", {
  g <- customGradientFun(c("navy", "firebrick"), function(x) x^2)
  expect_gradient_df(g)
  expect_error(customGradientFun(c("navy", "red"), function(x) sin(x * 2 * pi)),
               "\\[0, 1\\]|monotonic")
  expect_error(customGradientFun(c("navy", "red"), "notafunction"),
               "must be a function")
})

test_that("space = 'sRGB' reproduces the old interpolation", {
  lab <- sigmoidGradient2("navy", "white", width = 1)
  srgb <- sigmoidGradient2("navy", "white", width = 1, space = "sRGB")
  expect_false(identical(lab$color, srgb$color))
  expect_equal(lab$color[1], srgb$color[1])           # ends agree
  expect_equal(lab$color[101], srgb$color[101])
})

