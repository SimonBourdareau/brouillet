test_that("steepnessPresets lists both parameterisations", {
  tab <- steepnessPresets()
  expect_equal(nrow(tab), 10L)
  expect_true(all(c("preset", "sigma1", "sigma2", "center", "width",
                    "asymmetry") %in% names(tab)))
  expect_lt(tab$width[tab$preset == "extreme"], 0.1)
  expect_gt(tab$width[tab$preset == "very_low"], 0.8)
  expect_gt(tab$asymmetry[tab$preset == "extreme_longtail"], 0.5)
})

test_that("gradientParams accepts a steepness preset", {
  p <- gradientParams(c("navy", "firebrick"), steepness.preset = "high")
  tab <- steepnessPresets()
  row <- tab[tab$preset == "high", ]

  expect_equal(p$center, row$center)
  expect_equal(p$width, row$width)
  expect_equal(p$asymmetry, row$asymmetry)
  expect_equal(p$steepness.preset, "high")
  expect_true(all(isColor(buildGradient(p)$color)))
})

test_that("explicit curve arguments override the preset", {
  expect_equal(gradientParams(c("navy", "red"), steepness.preset = "high",
                              width = 0.6)$width, 0.6)
  expect_equal(gradientParams(c("navy", "red"), steepness.preset = "high",
                              center = 0.8)$center, 0.8)
  expect_equal(gradientParams(c("navy", "red"), steepness.preset = "high",
                              asymmetry = -0.3)$asymmetry, -0.3)

  # and the preset still supplies the ones not given
  p <- gradientParams(c("navy", "red"), steepness.preset = "high", width = 0.6)
  expect_equal(p$center, steepnessPresets()$center[steepnessPresets()$preset == "high"])
})

test_that("presets order themselves from broad to sharp", {
  widths <- vapply(c("very_low", "low", "medium", "high", "extreme"),
                   function(ps) gradientParams(c("navy", "red"),
                                               steepness.preset = ps)$width,
                   numeric(1))
  expect_true(all(diff(widths) < 0))
})

test_that("a preset works through templates and updates", {
  tmpl <- gradientTemplate(steepness.preset = "very_high")
  p <- withColors(tmpl, c("navy", "white", "firebrick"))
  expect_equal(p$width, steepnessPresets()$width[steepnessPresets()$preset == "very_high"])

  q <- updateGradient(p, resolution = 50)
  expect_equal(q$width, p$width)
  expect_equal(q$steepness.preset, "very_high")
})

test_that("preset arguments are validated", {
  expect_error(gradientParams(c("navy", "red"), steepness.preset = "nope"),
               "Invalid steepness preset")
  expect_error(gradientParams(c("navy", "red"), steepness.preset = "high",
                              curve.fun = function(x) x),
               "not both")
})

test_that("a preset gives the legacy shape, not the legacy colours", {
  # Same curve family, different interpolation space and rescaling, so the
  # colours differ; gradient_params() is the route to the old rendering.
  old <- buildGradient(gradient_params(c("navy", "firebrick"),
                                       steepness_preset = "medium"))
  new <- buildGradient(gradientParams(c("navy", "firebrick"),
                                      steepness.preset = "medium"))
  d <- sqrt(rowSums((t(grDevices::col2rgb(old$color)) -
                       t(grDevices::col2rgb(new$color)))^2))
  expect_gt(max(d), 10)

  # The modern curve reaches its end colours; the legacy one approaches them.
  # Visible on a broad preset, where the curve starts well above zero.
  broad_old <- buildGradient(gradient_params(c("navy", "firebrick"),
                                             steepness_preset = "very_low"))
  broad_new <- buildGradient(gradientParams(c("navy", "firebrick"),
                                            steepness.preset = "very_low"))
  expect_equal(toupper(broad_new$color[1]), "#000080")
  expect_equal(toupper(broad_new$color[nrow(broad_new)]), "#B22222")
  expect_false(identical(toupper(broad_old$color[1]), "#000080"))
  expect_false(identical(toupper(broad_old$color[nrow(broad_old)]), "#B22222"))
})
