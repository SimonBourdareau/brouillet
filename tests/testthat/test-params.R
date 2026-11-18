test_that("gradientParams validates and stores settings", {
  p <- gradientParams(colors = c("navy", "firebrick"), width = 0.3)
  expect_s3_class(p, "brouillet_gradient_params")
  expect_equal(p$gradient_mode, "sigmoid")
  expect_equal(p$space, "Lab")
  expect_equal(p$width, 0.3)

  expect_error(gradientParams(colors = "red"), "at least 2")
  expect_error(gradientParams(colors = c("red", "nope")), "Invalid colours")
  expect_error(gradientParams(colors = c("red", "white", "blue"),
                               midpoints = 1.5), "between 0 and 1")
  expect_error(gradientParams(colors = c("red", "blue"),
                               gradient_mode = "wiggly"), "gradient_mode")
  expect_error(gradientParams(colors = c("red", "blue"),
                               gradient_mode = "custom"), "must be a function")
  expect_warning(gradientParams(colors = c("red", "blue"), midpoints = 0.5),
                 "ignored")
})

test_that("width and rescale defaults depend on the mode", {
  expect_equal(gradientParams(c("red", "blue"))$width, 0.4)
  expect_equal(gradientParams(c("red", "blue"), gradient_mode = "linear")$width, 1)
  expect_true(gradientParams(c("red", "blue"))$rescale)
  expect_false(gradientParams(c("red", "blue"), gradient_mode = "linear")$rescale)
})

test_that("midpoints default to evenly spaced stops", {
  expect_equal(gradientParams(c("red", "white", "blue"))$midpoints, 0.5)
  expect_equal(gradientParams(c("red", "white", "blue", "black"))$midpoints,
               c(1 / 3, 2 / 3))
})

test_that("print method is informative and returns its input invisibly", {
  p <- gradientParams(colors = c("red", "white", "blue"), midpoints = 0.5)
  expect_output(print(p), "brouillet gradient")
  expect_output(print(p), "red -> white -> blue")
  expect_output(print(p), "Lab")
  expect_output(print(p), "center")
  capture.output(expect_invisible(print(p)))
})

test_that("buildGradient dispatches on colour count and mode", {
  for (mode in c("sigmoid", "linear")) {
    grads <- list(
      buildGradient(gradientParams(c("red", "blue"), gradient_mode = mode)),
      buildGradient(gradientParams(c("red", "white", "blue"),
                                     midpoints = 0.5, gradient_mode = mode)),
      buildGradient(gradientParams(c("red", "white", "blue", "green"),
                                     midpoints = c(0.3, 0.6), gradient_mode = mode))
    )
    for (g in grads) {
      expect_true(all(isColor(g$color)))
      expect_equal(range(g$value), c(0, 1))
      expect_false(is.unsorted(g$value))
    }
  }

  gc <- buildGradient(gradientParams(c("red", "blue"), gradient_mode = "custom",
                                       mapping_fun = function(x) sqrt(x)))
  expect_true(all(isColor(gc$color)))
})

test_that("midpoints are relative, so they follow the limits", {
  p <- gradientParams(colors = c("navy", "white", "firebrick"), midpoints = 0.25)
  g <- buildGradient(p, limits = c(0, 100))
  expect_equal(range(g$value), c(0, 100))
  expect_true(any(abs(g$value - 25) < 1e-6))
  # The interior colour sits at the interior stop, not the middle.
  at25 <- grDevices::col2rgb(g$color[which.min(abs(g$value - 25))])
  expect_true(all(at25 > 235))
})

test_that("reverseGradient mirrors the whole specification", {
  p <- gradientParams(colors = c("navy", "white", "firebrick"), midpoints = 0.4,
                       center = 0.3, asymmetry = 0.5,
                       low.clip.color = "grey80", high.clip.color = "black")
  r <- reverseGradient(p)
  expect_equal(r$colors, c("firebrick", "white", "navy"))
  expect_equal(r$midpoints, 0.6)
  expect_equal(r$center, 0.7)
  expect_equal(r$asymmetry, -0.5)
  expect_equal(r$low.clip.color, "black")
  expect_equal(r$high.clip.color, "grey80")
  expect_error(reverseGradient(list()), "brouillet_gradient_params")
})

test_that("reversing twice returns the original gradient", {
  p <- gradientParams(colors = c("navy", "white", "firebrick"), midpoints = 0.4,
                       center = 0.35, asymmetry = 0.4)
  expect_colors_close(buildGradient(reverseGradient(reverseGradient(p)))$color,
                      buildGradient(p)$color)
  # A reversed gradient is the mirror image of the original.
  expect_colors_close(buildGradient(reverseGradient(p))$color,
                      rev(buildGradient(p)$color))
})

test_that("clipping adds out-of-range stops", {
  g <- sigmoidGradient2("navy", "firebrick")
  cl <- clippingGradient(g, 0.2, 0.8, "grey80", "black")
  expect_equal(nrow(cl), sum(g$value >= 0.2 & g$value <= 0.8) + 2)
  expect_equal(cl$color[1], "grey80")
  expect_equal(cl$color[nrow(cl)], "black")
  expect_false(is.unsorted(cl$value))
  expect_error(clippingGradient(g, 0.2, 0.8), "clip colours")
  expect_error(clippingGradient(data.frame(a = 1), 0, 1, "grey", "black"),
               "'value' and 'color'")
})

test_that("blended clipping ramps into the clip colours", {
  g <- sigmoidGradient2("navy", "firebrick")
  bl <- blendingClippingGradient(g, 0, 1, "grey80", "black")
  expect_gt(nrow(bl), nrow(g))
  expect_false(is.unsorted(bl$value))
  expect_equal(toupper(bl$color[nrow(bl)]), "#000000")
})

test_that("only one clip colour is enough", {
  p <- gradientParams(c("navy", "firebrick"), high.clip.color = "black")
  g <- buildGradient(p)
  expect_equal(g$color[nrow(g)], "black")
  expect_true(all(isColor(g$color)))
})

test_that("scale constructors produce usable ggplot2 scales", {
  skip_if_not_installed("ggplot2")

  p <- gradientParams(colors = c("navy", "white", "firebrick"), midpoints = 0.5)
  expect_s3_class(asScaleFillGradient(p), "Scale")
  expect_s3_class(asScaleColorGradient(p, limits = c(-2, 2)), "Scale")

  pc <- gradientParams(colors = c("navy", "white", "firebrick"), midpoints = 0.5,
                        low.clip.color = "grey80", high.clip.color = "black")
  expect_s3_class(asScaleFillGradient(pc), "Scale")

  expect_error(asScaleFillGradient(p, limits = c(1, 1)), "limits")
  expect_error(asScaleFillGradient(p, na.value = "nope"), "na.value")
  expect_error(asScaleFillGradient(p, clip.width = 2), "clip.width")
  expect_error(asScaleFillGradient(list()), "brouillet_gradient_params")
})

test_that("scale breakpoints are strictly increasing", {
  p <- gradientParams(colors = c("navy", "white", "firebrick"), midpoints = 0.5,
                       low.clip.color = "grey80", high.clip.color = "black")
  pieces <- brouillet:::.scale_pieces(p, c(0, 1), TRUE, 0.02)
  expect_true(all(diff(pieces$values) > 0))
  expect_equal(length(pieces$values), length(pieces$colours))
  expect_equal(pieces$limits, range(pieces$values))
})

test_that("previewGradient returns a ggplot", {
  skip_if_not_installed("ggplot2")
  p <- gradientParams(colors = c("navy", "white", "firebrick"), midpoints = 0.5)
  expect_s3_class(previewGradient(p), "ggplot")
  expect_warning(previewGradient(p, n_ticks = 1), "n_ticks")
})

test_that("clip colours reach values far outside the limits", {
  skip_if_not_installed("ggplot2")

  pc <- gradientParams(c("navy", "white", "firebrick"), midpoints = 0.5,
                        low.clip.color = "grey35", high.clip.color = "gold")
  sc <- asScaleFillGradient(pc, limits = c(-2, 2))

  # Squished, so anything below/above the limits gets the clip colour rather
  # than falling through to na.value once it passes the clip band.
  expect_equal(toupper(sc$map(-9)), "#595959")   # grey35
  expect_equal(toupper(sc$map(9)), "#FFD700")    # gold
  expect_equal(toupper(sc$map(0)), "#FFFFFF")

  # A user-supplied oob still wins.
  sc2 <- asScaleFillGradient(pc, limits = c(-2, 2), na.value = "grey95",
                                oob = scales::oob_censor)
  expect_equal(sc2$map(9), "grey95")
})

test_that("a colourless template can be created and completed", {
  tmpl <- gradientParams(colors = NULL, width = 0.3, high.clip.color = "black")
  expect_s3_class(tmpl, "brouillet_gradient_params")
  expect_null(tmpl$colors)
  expect_output(print(tmpl), "template")
  expect_output(print(tmpl), "not set")

  # Unusable until completed, with an actionable message rather than a crash.
  expect_error(buildGradient(tmpl), "no colours")
  expect_error(asColorFun(tmpl), "no colours")
  expect_error(asScaleFillGradient(tmpl), "no colours")

  p <- updateGradient(tmpl, colors = c("navy", "white", "firebrick"))
  expect_equal(p$colors, c("navy", "white", "firebrick"))
  expect_equal(p$width, 0.3)              # template settings survive
  expect_equal(p$high.clip.color, "black")
  expect_equal(p$midpoints, 0.5)          # filled in by default
  expect_true(all(isColor(buildGradient(p)$color)))
})

test_that("templates still validate everything except the palette", {
  expect_error(gradientParams(colors = NULL, width = -1), "positive")
  expect_error(gradientParams(colors = NULL, midpoints = 1.5), "between 0 and 1")
  expect_error(gradientParams(colors = NULL, space = "hsv"), "Lab")
  # Midpoints that cannot fit the eventual palette are dropped on completion,
  # matching gradientParams()' own behaviour for 2-colour gradients.
  tmpl <- gradientParams(colors = NULL, midpoints = c(0.3, 0.6))
  expect_warning(p2 <- updateGradient(tmpl, colors = c("red", "blue")), "dropped")
  expect_null(p2$midpoints)
  # But they are honoured when the palette does fit them.
  p4 <- updateGradient(tmpl, colors = c("red", "white", "blue", "black"))
  expect_equal(p4$midpoints, c(0.3, 0.6))
})

test_that("updateGradient replaces fields and revalidates", {
  p <- gradientParams(c("navy", "white", "firebrick"), midpoints = 0.5, width = 0.4)

  expect_equal(updateGradient(p, width = 0.1)$width, 0.1)
  # 3 -> 2 colours: the stored midpoint no longer applies, so it is dropped.
  expect_warning(p2 <- updateGradient(p, colors = c("black", "white")), "dropped")
  expect_equal(p2$colors, c("black", "white"))
  expect_null(p2$midpoints)
  expect_error(updateGradient(p, width = -2), "positive")
  expect_error(updateGradient(p, wdith = 0.1), "Unknown gradient field")
  expect_error(updateGradient(p, 0.1), "must be named")
  expect_error(updateGradient(list(), width = 0.1), "brouillet_gradient_params")

  # Resetting a field to NULL restores the default.
  pc <- gradientParams(c("navy", "firebrick"), high.clip.color = "black")
  expect_null(updateGradient(pc, high.clip.color = NULL)$high.clip.color)

  # Stale midpoints are dropped rather than erroring on a length mismatch.
  expect_warning(u <- updateGradient(p, colors = c("navy", "white",
                                                   "firebrick", "black")),
                 "dropped")
  expect_equal(u$midpoints, c(1 / 3, 2 / 3))
  # Supplying both at once is the quiet path.
  expect_silent(updateGradient(p, colors = c("navy", "white", "firebrick", "black"),
                                midpoints = c(0.4, 0.8)))
})



test_that("the exported surface is camelCase apart from gradient_params()", {
  ex <- getNamespaceExports("brouillet")
  ex <- ex[!grepl("^print\\.", ex)]
  # gradient_params() keeps its original name on purpose: it exists so code
  # written against the pre-0.2 API keeps running.
  expect_equal(ex[grepl("_", ex)], "gradient_params")
  expect_false(any(c("as_color_fun", "sigmoid_gradient2") %in% ex))
  expect_false("sigma1" %in% names(formals(sigmoidGradient2)))
  # gradientParams() does take steepness_preset, as a shorthand for
  # center/width/asymmetry rather than as the legacy sigma pair.
  expect_true("steepness_preset" %in% names(formals(gradientParams)))
  expect_false("sigma1" %in% names(formals(gradientParams)))
})

test_that("S3 class strings are unchanged, so inherits() still works", {
  p <- gradientParams(c("navy", "firebrick"))
  expect_s3_class(p, "brouillet_gradient_params")
  expect_s3_class(asColorFun(p), "brouillet_color_fun")
  expect_s3_class(gradientTemplate(width = 0.2), "brouillet_gradient_template")
})

test_that("previewGradient shows clip colours by drawing past the limits", {
  skip_if_not_installed("ggplot2")

  plain <- gradientParams(c("navy", "firebrick"))
  clipped <- gradientParams(c("navy", "firebrick"),
                            low.clip.color = "grey35", high.clip.color = "gold")

  rng <- function(p, ...) range(ggplot2::ggplot_build(previewGradient(p, ...))$data[[1]]$x)

  expect_equal(rng(plain, limits = c(0, 1)), c(0, 1))
  expect_lt(rng(clipped, limits = c(0, 1))[1], 0)
  expect_gt(rng(clipped, limits = c(0, 1))[2], 1)

  fills <- ggplot2::ggplot_build(previewGradient(clipped))$data[[1]]$fill
  expect_colors_close(fills[1], "grey35")
  expect_colors_close(fills[length(fills)], "gold")

  expect_equal(rng(clipped, limits = c(0, 1), clip.pad = 0), c(0, 1))
  expect_error(previewGradient(clipped, clip.pad = 2), "clip.pad")
})

test_that("blend.clip is not interrupted by hard clip bands", {
  skip_if_not_installed("ggplot2")

  p <- gradientParams(c("navy", "white", "firebrick"), midpoints = 0.5,
                      low.clip.color = "grey35", high.clip.color = "gold",
                      blend.clip = TRUE)

  fills <- ggplot2::ggplot_build(previewGradient(p, limits = c(-2, 2)))$data[[1]]$fill
  gold <- as.numeric(grDevices::col2rgb("gold"))
  d <- apply(grDevices::col2rgb(fills), 2, function(v) sqrt(sum((v - gold)^2)))

  # Across the upper blend the colour must approach gold without reversing.
  upper <- d[round(length(d) * 0.94):length(d)]
  expect_true(all(diff(upper) <= 1))
})
