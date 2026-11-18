lum <- function(cols) {
  rgb <- grDevices::col2rgb(cols)
  as.numeric(0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ])
}

test_that("asColorFun maps values to colours over the given limits", {
  p <- gradientParams(c("black", "white"), gradient_mode = "linear")
  f <- asColorFun(p, limits = c(0, 10))

  expect_s3_class(f, "brouillet_color_fun")
  expect_equal(toupper(f(0)), "#000000")
  expect_equal(toupper(f(10)), "#FFFFFF")
  expect_length(f(1:10), 10)
  expect_true(all(diff(lum(f(seq(0, 10, 0.5)))) >= -1e-6))
})

test_that("asColorFun handles NA and out-of-range values", {
  p <- gradientParams(c("black", "white"), gradient_mode = "linear")

  f <- asColorFun(p, limits = c(0, 1), na.color = "grey50")
  expect_equal(f(NA_real_), "grey50")
  expect_equal(f(c(NA, NaN, Inf, -Inf)), rep("grey50", 4))
  # Without clip colours, out-of-range clamps to the end colours.
  expect_equal(toupper(f(-5)), "#000000")
  expect_equal(toupper(f(5)), "#FFFFFF")

  pc <- gradientParams(c("black", "white"), gradient_mode = "linear",
                        low.clip.color = "blue", high.clip.color = "red")
  fc <- asColorFun(pc)
  expect_equal(fc(-0.01), "blue")
  expect_equal(fc(1.01), "red")
  expect_equal(toupper(fc(0)), "#000000")
})

test_that("asColorFun agrees with buildGradient on the same grid", {
  p <- gradientParams(c("navy", "white", "firebrick"), midpoints = 0.5,
                       width = 0.3, resolution = 50)
  g <- buildGradient(p, limits = c(-2, 2))
  f <- asColorFun(p, limits = c(-2, 2))
  expect_colors_close(f(g$value), g$color)
})

test_that("asColorFun handles empty and zero-length input", {
  p <- gradientParams(c("black", "white"))
  f <- asColorFun(p)
  expect_equal(f(numeric(0)), character(0))
})

test_that("asColorFun validates its arguments", {
  p <- gradientParams(c("black", "white"))
  expect_error(asColorFun(list()), "brouillet_gradient_params")
  expect_error(asColorFun(p, limits = c(1, 0)), "limits")
  expect_error(asColorFun(p, na.color = "nope"), "na.color")
})

test_that("print method for the colour function is informative", {
  p <- gradientParams(c("black", "white"))
  expect_output(print(asColorFun(p, limits = c(-1, 1))), "colour function")
  expect_output(print(asColorFun(p, limits = c(-1, 1))), "brouillet gradient")
})

test_that("Lab is the default interpolation space and differs from sRGB", {
  lab <- asColorFun(gradientParams(c("navy", "white"),
                                      gradient_mode = "linear"))(0.5)
  srgb <- asColorFun(gradientParams(c("navy", "white"), space = "sRGB",
                                       gradient_mode = "linear"))(0.5)
  expect_false(identical(lab, srgb))
  # A naive RGB average of navy and white gives equal R and G; interpolating
  # through Lab preserves hue better and lands well to the red side of that.
  expect_gt(grDevices::col2rgb(lab)[1, 1] - grDevices::col2rgb(srgb)[1, 1], 5)
})

test_that("Lab interpolation of black to white is perceptually even", {
  f <- asColorFun(gradientParams(c("black", "white"), gradient_mode = "linear"))
  l <- lum(f(seq(0, 1, length.out = 65)))
  d_lab <- diff(l)

  g <- asColorFun(gradientParams(c("black", "white"), space = "sRGB",
                                    gradient_mode = "linear"))
  d_srgb <- diff(lum(g(seq(0, 1, length.out = 65))))

  # sRGB steps are equal in code value but not in perceived lightness, so Lab
  # should show larger variation in raw luminance steps, not smaller.
  expect_true(sd(d_srgb) < sd(d_lab))
  expect_true(all(d_lab > 0))
})

test_that("spline interpolation stays inside the sRGB cube", {
  p <- gradientParams(c("navy", "white", "firebrick", "black"),
                       midpoints = c(0.4, 0.8), interpolate = "spline")
  cols <- asColorFun(p)(seq(0, 1, length.out = 200))
  expect_true(all(isColor(cols)))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
})

test_that("interpolation arguments are validated", {
  expect_error(gradientParams(c("black", "white"), space = "hsv"), "Lab")
  expect_error(gradientParams(c("black", "white"), interpolate = "cubic"),
               "linear")
})

test_that("the default Lab path matches ggplot2's own interpolation exactly", {
  skip_if_not_installed("scales")

  cols <- c("navy", "white", "firebrick")
  t <- seq(0, 1, length.out = 1001)

  # scale_*_gradientn() interpolates supplied stops with scales::colour_ramp().
  # Delegating to it means asColorFun() and the ggplot2 scale cannot drift.
  f <- asColorFun(gradientParams(cols, midpoints = 0.5,
                                    gradient_mode = "linear"))
  expect_identical(f(t), scales::gradient_n_pal(cols)(t))
})

test_that("endpoints survive the round trip exactly", {
  # rgb() truncates, so a Lab round trip that returns 127.9999 for navy's 128
  # used to shift every stop one code value down.
  for (space in c("Lab", "sRGB")) {
    g <- sigmoidGradient2("navy", "firebrick", space = space)
    expect_equal(toupper(g$color[1]), "#000080")
    expect_equal(toupper(g$color[nrow(g)]), "#B22222")
  }
  g <- sigmoidGradient2("navy", "firebrick", interpolate = "spline")
  expect_equal(toupper(g$color[1]), "#000080")
})

test_that("blend.clip fades the clip colour in, matching the stop table", {
  hard <- asColorFun(gradientParams(c("navy", "firebrick"),
                                       low.clip.color = "grey35",
                                       high.clip.color = "gold"))
  soft <- asColorFun(gradientParams(c("navy", "firebrick"),
                                       low.clip.color = "grey35",
                                       high.clip.color = "gold",
                                       blend.clip = TRUE))

  # Far outside, both give the pure clip colour. The hard path passes the
  # colour string through untouched, the blend path returns hex, so compare
  # as colours rather than as strings.
  expect_colors_close(hard(-5), soft(-5))
  expect_colors_close(soft(-5), "grey35")
  expect_colors_close(soft(9), "gold")

  # Just outside, blending gives something between the end and the clip colour.
  expect_false(identical(hard(-0.02), soft(-0.02)))
  expect_false(identical(hard(1.02), soft(1.02)))

  # And the blend is monotone in distance from the limit.
  d <- vapply(seq(0, 0.1, length.out = 20), function(k) {
    as.numeric(grDevices::col2rgb(soft(1 + k))[1, ])
  }, numeric(1))
  expect_true(all(diff(d) >= -1))
})

test_that("alpha is interpolated when present and absent when not", {
  alpha_of <- function(cols) as.numeric(grDevices::col2rgb(cols, alpha = TRUE)[4, ])

  for (args in list(list(), list(space = "sRGB"), list(interpolate = "spline"))) {
    g <- do.call(sigmoidGradient2,
                 c(list("#FF000000", "#FF0000FF", resolution = 20), args))
    a <- alpha_of(g$color)
    expect_equal(a[1], 0)
    expect_equal(a[length(a)], 255)
    expect_true(all(diff(a) >= 0))            # monotone fade
    expect_gt(length(unique(a)), 5)           # not collapsed to one colour
  }

  # Opaque palettes keep returning 6-digit hex.
  opaque <- asColorFun(gradientParams(c("navy", "firebrick")))(c(0, 0.5, 1))
  expect_true(all(nchar(opaque) == 7))
  expect_true(all(alpha_of(opaque) == 255))
})

test_that("alpha survives every output form", {
  p <- gradientParams(c("#00000000", "#0000FFFF"))
  alpha_of <- function(cols) as.numeric(grDevices::col2rgb(cols, alpha = TRUE)[4, ])

  expect_equal(alpha_of(asColorFun(p)(0.5)), 128, tolerance = 2)
  expect_equal(alpha_of(buildGradient(p)$color[51]), 128, tolerance = 2)

  skip_if_not_installed("ggplot2")
  sc <- asScaleFillGradient(p, limits = c(0, 1))
  expect_equal(alpha_of(sc$map(0.5)), 128, tolerance = 3)
})
