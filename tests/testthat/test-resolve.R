test_that("hasClipColors reports what the specification defines", {
  expect_false(hasClipColors(gradientParams(c("navy", "firebrick"))))
  expect_true(hasClipColors(gradientParams(c("navy", "firebrick"),
                                           high.clip.color = "gold")))
  expect_true(hasClipColors(gradientTemplate(low.clip.color = "grey35")))
  expect_error(hasClipColors(list()), "gradient specification")
})

test_that("resolveGradient builds from nothing", {
  p <- resolveGradient(NULL, c("white", "pink", "red"), limits = c(0, 1),
                       midpoint = 0.5)
  expect_equal(p$colors, c("white", "pink", "red"))
  expect_equal(p$midpoints, 0.5)
  expect_equal(p$gradient_mode, "linear")
})

test_that("a data-unit midpoint becomes a fraction", {
  p <- resolveGradient(NULL, c("white", "pink", "red"), limits = c(0, 100),
                       midpoint = 25)
  expect_equal(p$midpoints, 0.25)

  p2 <- resolveGradient(NULL, c("white", "pink", "red"), limits = c(-2, 2),
                        midpoint = 0)
  expect_equal(p2$midpoints, 0.5)
})

test_that("a midpoint on or outside a limit drops the middle colour", {
  expect_message(
    p <- resolveGradient(NULL, c("white", "pink", "red"), limits = c(0, 1),
                         midpoint = 0),
    "sits on a scale limit"
  )
  expect_equal(p$colors, c("white", "red"))
  expect_null(p$midpoints)

  expect_silent(resolveGradient(NULL, c("white", "pink", "red"), limits = c(0, 1),
                                midpoint = NA, quiet = TRUE))

  # A NULL middle colour collapses the vector, so no message is needed.
  expect_silent(resolveGradient(NULL, c("white", NULL, "red"), limits = c(0, 1),
                                midpoint = 0.5))
})

test_that("templates and colourless specifications are completed", {
  tmpl <- gradientTemplate(width = 0.2, high.clip.color = "gold")
  p <- resolveGradient(tmpl, c("white", "pink", "red"), limits = c(0, 100),
                       midpoint = 50)
  expect_equal(p$colors, c("white", "pink", "red"))
  expect_equal(p$midpoints, 0.5)
  expect_equal(p$width, 0.2)
  expect_equal(p$low.clip.color, "white")
})

test_that("a caller's own palette wins", {
  own <- gradientParams(c("navy", "gold"))
  p <- resolveGradient(own, c("white", "pink", "red"), limits = c(0, 1),
                       midpoint = 0.5)
  expect_equal(p$colors, c("navy", "gold"))

  own3 <- gradientParams(c("navy", "white", "gold"), midpoints = 0.9)
  p3 <- resolveGradient(own3, c("white", "pink", "red"), limits = c(0, 1),
                        midpoint = 0.25)
  expect_equal(p3$midpoints, 0.25)
})

test_that("one clip colour is mirrored to the other side", {
  p <- resolveGradient(gradientParams(c("navy", "gold"), low.clip.color = "grey35"),
                       c("white", "red"), limits = c(0, 1))
  expect_equal(p$high.clip.color, "gold")

  q <- resolveGradient(gradientParams(c("navy", "gold"), high.clip.color = "black"),
                       c("white", "red"), limits = c(0, 1))
  expect_equal(q$low.clip.color, "navy")
})

test_that("legacy specifications keep midpoints in data units", {
  old <- gradient_params(colors = NULL, steepness_preset = "high")
  p <- resolveGradient(old, c("white", "pink", "red"), limits = c(0, 100),
                       midpoint = 50)
  expect_true(isTRUE(p$legacy))
  expect_equal(p$midpoints, 50)

  g <- buildGradient(p, limits = c(0, 100))
  at50 <- grDevices::col2rgb(g$color[which.min(abs(g$value - 50))])
  expect_colors_close(grDevices::rgb(t(at50), maxColorValue = 255), "pink", tol = 40)
})

test_that("resolveGradient validates its arguments", {
  expect_error(resolveGradient(NULL, "white", limits = c(0, 1)), "at least 2")
  expect_error(resolveGradient(NULL, c("white", "red"), midpoint = 0.5), "'limits' is required")
  expect_error(resolveGradient(list(), c("white", "red"), limits = c(0, 1)),
               "gradientParams\\(\\)")
})
