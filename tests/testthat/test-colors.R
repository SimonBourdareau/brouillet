test_that("isColor recognises valid and invalid colours", {
  expect_true(isColor("red"))
  expect_true(isColor("#FF0000"))
  expect_equal(isColor(c("red", "notacolor", NA)), c(TRUE, FALSE, FALSE))
  expect_equal(isColor(character(0)), logical(0))
})

test_that("colorComplementary flips the hue", {
  expect_equal(toupper(colorComplementary("red")), "#00FFFF")
  # Complement of a complement returns the original hue.
  expect_equal(colorComplementary(colorComplementary("red")), "#FF0000")
})

test_that("colorWheel returns the requested number of colours", {
  w <- colorWheel("red", 12)
  expect_length(w, 12)
  expect_true(all(isColor(w)))
  expect_equal(toupper(w[1]), "#FF0000")
  expect_error(colorWheel("red", 2.5), "positive integer")
})

test_that("colorWheel preserves an 8-digit alpha channel", {
  w <- colorWheel("#FF000080", 4)
  expect_true(all(nchar(w) == 9))
})

test_that("colorSpread returns spread - 1 colours", {
  expect_length(colorSpread("blue", 4), 3)
  expect_error(colorSpread("blue", 1), ">= 2")
  expect_warning(colorSpread("blue", 5, resolution = 2), "at least")
})

test_that("colorBlend interpolates and clips weights", {
  expect_equal(toupper(colorBlend("red", "blue", 1)), "#FF0000")
  expect_equal(toupper(colorBlend("red", "blue", 0)), "#0000FF")
  expect_equal(toupper(colorBlend("black", "white", 0.5)), "#808080")
  expect_warning(colorBlend("red", "blue", 2), "clipped")
})

test_that("colorUpdate accepts all documented color2 forms", {
  expect_true(isColor(colorUpdate("red", "blue", shift = 50)))
  expect_true(isColor(colorUpdate("red", "complementary", shift = 30)))
  expect_true(isColor(colorUpdate("red", shift = 20)))
  # Numeric color2 goes through colorSpread(); this was broken before.
  expect_true(isColor(colorUpdate("red", 3, shift = 40)))
  expect_error(colorUpdate("notacolor"), "not a single valid colour")
  expect_error(colorUpdate("red", "nope"), "valid colour")
})

test_that("colorBlend can interpolate in Lab", {
  srgb <- colorBlend("black", "white", 0.5)
  lab <- colorBlend("black", "white", 0.5, space = "Lab")
  expect_equal(toupper(srgb), "#808080")
  # Lab's midpoint is mid-lightness, which is darker in code value than 50% grey.
  expect_lt(grDevices::col2rgb(lab)[1, 1], grDevices::col2rgb(srgb)[1, 1])
  expect_error(colorBlend("black", "white", space = "hsv"), "Lab")
})

test_that("colorUpdate honours the interpolation space", {
  expect_false(identical(colorUpdate("navy", "white", shift = 50),
                         colorUpdate("navy", "white", shift = 50, space = "sRGB")))
  expect_equal(toupper(colorUpdate("navy", "white", shift = 0)), "#FFFFFF")
  expect_equal(toupper(colorUpdate("navy", "white", shift = 100)), "#000080")
})
