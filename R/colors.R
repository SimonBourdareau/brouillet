#' Check whether a string is a valid colour
#'
#' @description Tests whether each element can be converted to RGB by
#'   [grDevices::col2rgb()]. Vectorised; `NA` is never a valid colour.
#'
#' @param color Character (or numeric palette index) vector of candidate colours.
#' @return Logical vector the same length as `color`.
#' @export
#' @examples
#' isColor("red")
#' isColor(c("#FF0000", "notacolor"))
isColor <- function(color) {
  if (length(color) == 0) return(logical(0))
  vapply(color, function(x) {
    # col2rgb() accepts NA silently, so screen it out first.
    if (length(x) != 1 || is.na(x)) return(FALSE)
    tryCatch({
      col2rgb(x)
      TRUE
    }, error = function(e) FALSE)
  }, logical(1), USE.NAMES = FALSE)
}

#' Shift a colour towards a second colour
#'
#' @description Interpolates along a colour ramp between `color` and a second
#'   colour, returning the entry at the requested position.
#'
#' @param color Primary colour.
#' @param color2 Secondary colour. One of: a valid colour; the string
#'   `"complementary"` (uses [colorComplementary()]); a single number `>= 2`
#'   (uses the [colorSpread()] palette of `color`); or `NULL` for white.
#' @param shift Position along the ramp, on the same scale as `resolution`
#'   (0 = fully `color2`, `resolution` = fully `color`).
#' @param resolution Number of steps in the ramp (>= 2).
#' @param space Colour interpolation space, `"Lab"` (default) or `"sRGB"`.
#' @return A single hexadecimal colour string.
#' @export
#' @examples
#' colorUpdate("red", "blue", shift = 50)
#' colorUpdate("red", "complementary", shift = 30)
#' colorUpdate("red", shift = 20) # towards white
colorUpdate <- function(color, color2 = NULL, shift = 50, resolution = 100,
                        space = "Lab") {

  .check_color(color, "color")

  if (!is.numeric(shift) || length(shift) != 1 || is.na(shift)) {
    stop("'shift' must be a single numeric value.", call. = FALSE)
  }

  resolution <- .fix_resolution(resolution, min_res = 2)
  shift <- max(0, min(resolution, shift))

  if (is.null(color2)) {
    colorVector <- c("white", color)

  } else if (is.character(color2) && length(color2) == 1) {

    if (identical(color2, "complementary")) {
      colorVector <- c(colorComplementary(color), color)
    } else if (isColor(color2)) {
      colorVector <- c(color2, color)
    } else {
      stop("'color2' must be a valid colour or \"complementary\".", call. = FALSE)
    }

  } else if (is.numeric(color2) && length(color2) == 1 && !is.na(color2) && color2 >= 2) {
    # Ramp from the base colour through its evenly spread wheel neighbours.
    colorVector <- c(color, colorSpread(color, spread = color2))

  } else {
    stop("'color2' must be a colour, \"complementary\", a single number >= 2, or NULL.",
         call. = FALSE)
  }

  .make_ramp(colorVector, space = space)(shift / resolution)
}

#' Generate a colour wheel
#'
#' @description Creates `num` colours evenly spaced in hue around the HSV
#'   wheel, starting at `color` and preserving its saturation and value.
#'
#' @param color Base colour.
#' @param num Number of colours to generate (positive integer).
#' @return Character vector of hexadecimal colours.
#' @export
#' @examples
#' colorWheel("red", 12)
colorWheel <- function(color, num) {

  .check_color(color, "color")

  if (!is.numeric(num) || length(num) != 1 || is.na(num) ||
      num < 1 || num != round(num)) {
    stop("'num' must be a single positive integer.", call. = FALSE)
  }
  num <- as.integer(num)

  hsv_col <- rgb2hsv(col2rgb(color))[, 1]

  hues <- seq(hsv_col[1], hsv_col[1] + 1 - 1 / num, length.out = num)
  hues[hues > 1] <- hues[hues > 1] - 1

  colors <- hsv(hues, hsv_col[2], hsv_col[3])

  # Carry an 8-digit hex alpha channel through, if present.
  if (substr(color, 1, 1) == "#" && nchar(color) == 9) {
    colors <- paste0(colors, substr(color, 8, 9))
  }

  colors
}

#' Find the complementary colour
#'
#' @description Returns the colour on the opposite side of the HSV hue wheel.
#'
#' @param color Input colour.
#' @return A single hexadecimal colour string.
#' @export
#' @examples
#' colorComplementary("red")
colorComplementary <- function(color) {

  .check_color(color, "color")

  hsv_col <- rgb2hsv(col2rgb(color))[, 1]

  comp_hue <- hsv_col[1] + 0.5
  if (comp_hue > 1) comp_hue <- comp_hue - 1

  comp_color <- hsv(comp_hue, hsv_col[2], hsv_col[3])

  if (substr(color, 1, 1) == "#" && nchar(color) == 9) {
    comp_color <- paste0(comp_color, substr(color, 8, 9))
  }

  comp_color
}

#' Generate evenly spread colours around the wheel
#'
#' @description Divides the hue wheel into `spread` sections and returns the
#'   `spread - 1` colours that are not the base colour.
#'
#' @param color Base colour.
#' @param spread Number of divisions of the wheel (>= 2).
#' @param resolution Resolution of the underlying colour wheel; coerced up to
#'   `spread * 10` when smaller than `spread`.
#' @return Character vector of `spread - 1` hexadecimal colours.
#' @export
#' @examples
#' colorSpread("blue", 4) # 3 colours, blue excluded
colorSpread <- function(color, spread = 3, resolution = 100) {

  .check_color(color, "color")

  if (!is.numeric(spread) || length(spread) != 1 || is.na(spread) || spread < 2) {
    stop("'spread' must be a single number >= 2.", call. = FALSE)
  }
  spread <- floor(spread)

  if (!is.numeric(resolution) || length(resolution) != 1 ||
      is.na(resolution) || resolution < spread) {
    resolution <- spread * 10
    warning("'resolution' must be at least 'spread'. Using ", resolution,
            ".", call. = FALSE)
  }
  resolution <- floor(resolution)

  tmp_cols <- colorWheel(color, resolution)
  step <- floor(resolution / spread)

  tmp_cols[seq(step, resolution - step, by = step)]
}

#' Blend two colours
#'
#' @description Interpolation between two colours. Alpha channels are ignored.
#'
#' @param color1 First colour.
#' @param color2 Second colour.
#' @param weight Weight given to `color1`, between 0 and 1.
#' @param space Interpolation space. Defaults to `"sRGB"` for this low-level
#'   helper, unlike the gradient builders, which default to `"Lab"`.
#' @return A single hexadecimal colour string.
#' @export
#' @examples
#' colorBlend("red", "blue", 0.5)
#' colorBlend("black", "white", 0.5, space = "Lab")
colorBlend <- function(color1, color2, weight = 0.5, space = "sRGB") {

  .check_color(color1, "color1")
  .check_color(color2, "color2")

  if (!is.numeric(weight) || length(weight) != 1 || is.na(weight)) {
    stop("'weight' must be a single numeric value.", call. = FALSE)
  }

  if (weight < 0 || weight > 1) {
    warning("'weight' should be between 0 and 1; value was clipped.", call. = FALSE)
    weight <- max(0, min(1, weight))
  }

  if (.check_space(space) == "Lab") {
    return(.make_ramp(c(color2, color1), space = "Lab")(weight))
  }

  rgb1 <- col2rgb(color1)
  rgb2 <- col2rgb(color2)

  blended <- round(rgb1 * weight + rgb2 * (1 - weight))

  # round(), not rgb()'s implicit truncation: a 50/50 blend of black and white
  # is 127.5, which should be #808080 rather than #7F7F7F.
  rgb(blended[1], blended[2], blended[3], maxColorValue = 255)
}
