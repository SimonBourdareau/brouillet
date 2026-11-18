test_that("clippingGradient accepts a specification and returns one", {
  p <- gradientParams(c("navy", "white", "firebrick"), midpoints = 0.5)
  q <- clippingGradient(p, low.clip.color = "grey35", high.clip.color = "gold")

  expect_s3_class(q, "brouillet_gradient_params")
  expect_equal(q$low.clip.color, "grey35")
  expect_equal(q$high.clip.color, "gold")
  expect_false(q$blend.clip)

  # Equivalent to setting the fields directly.
  direct <- updateGradient(p, low.clip.color = "grey35", high.clip.color = "gold")
  expect_identical(buildGradient(q)$color, buildGradient(direct)$color)
})

test_that("blendingClippingGradient sets blend.clip on a specification", {
  p <- gradientParams(c("navy", "firebrick"))
  b <- blendingClippingGradient(p, low.clip.color = "grey35",
                                high.clip.color = "gold")
  expect_true(b$blend.clip)
  expect_true(hasClipColors(b))
})

test_that("templates keep their class and settings", {
  t0 <- gradientTemplate(width = 0.2, center = 0.3)
  t1 <- clippingGradient(t0, high.clip.color = "gold")

  expect_s3_class(t1, "brouillet_gradient_template")
  expect_equal(t1$width, 0.2)
  expect_equal(t1$center, 0.3)
  expect_equal(t1$high.clip.color, "gold")
  expect_true(all(isColor(buildGradient(withColors(t1, c("navy", "red")))$color)))
})

test_that("one clip colour is enough for a specification", {
  p <- gradientParams(c("navy", "firebrick"))
  expect_equal(clippingGradient(p, high.clip.color = "gold")$high.clip.color, "gold")
  expect_null(clippingGradient(p, high.clip.color = "gold")$low.clip.color)
  expect_error(clippingGradient(p), "at least one")
})

test_that("the windowing arguments are rejected for a specification", {
  p <- gradientParams(c("navy", "firebrick"))
  expect_error(clippingGradient(p, 0.2, 0.8, "grey", "black"), "no data range")
  expect_error(blendingClippingGradient(p, 0.2, 0.8, "grey", "black"), "no data range")
})

test_that("the stop-table form is unchanged", {
  g <- buildGradient(gradientParams(c("navy", "firebrick")))
  cl <- clippingGradient(g, 0.2, 0.8, "grey80", "black")

  expect_s3_class(cl, "data.frame")
  expect_equal(nrow(cl), sum(g$value >= 0.2 & g$value <= 0.8) + 2)
  expect_equal(cl$color[1], "grey80")
  expect_equal(cl$color[nrow(cl)], "black")

  bl <- blendingClippingGradient(g, 0, 1, "grey80", "black")
  expect_s3_class(bl, "data.frame")
  expect_gt(nrow(bl), nrow(g))
})
