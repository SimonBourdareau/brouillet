## The single interpolation primitive used by every gradient in the package.
## Everything else decides *where* to sample; this decides *how* colours are
## mixed. Default is CIE Lab, because sRGB interpolation is perceptually
## non-uniform and produces false structure in quantitative colour scales.

#' Build a colour ramp function
#'
#' @param colors Two or more colours.
#' @param space `"Lab"` (perceptual, default) or `"sRGB"`.
#' @param interpolate `"linear"` or `"spline"`.
#' @return A function mapping `t` in \[0, 1\] to hex colours. Alpha is
#'   interpolated alongside the colour when any input colour is translucent,
#'   and omitted from the output otherwise, so opaque palettes keep returning
#'   6-digit hex.
#' @noRd
.make_ramp <- function(colors, space = "Lab", interpolate = "linear") {

  .check_colors(colors, min_n = 2)
  space <- .check_space(space)
  interpolate <- .check_interpolate(interpolate)

  # Only carry an alpha channel when there is one to carry: emitting "#000080FF"
  # for an opaque palette would be noisy, and silently discarding a translucent
  # one collapses e.g. transparent-red -> opaque-red into a flat colour.
  keep_alpha <- any(grDevices::col2rgb(colors, alpha = TRUE)[4, ] < 255)

  # The default path delegates to scales::colour_ramp(), which is exactly what
  # ggplot2's scale_*_gradientn() uses. That makes asColorFun() and the
  # ggplot2 scales agree colour-for-colour rather than merely closely, and it
  # is faster (farver, in C) than a convertColor() round trip.
  if (space == "Lab" && interpolate == "linear") {
    f <- scales::colour_ramp(colors, alpha = keep_alpha)
    return(function(t) {
      t <- .clamp01(t)
      if (!length(t)) return(character(0))
      f(t)
    })
  }

  f <- grDevices::colorRamp(
    colors,
    space = if (space == "Lab") "Lab" else "rgb",
    interpolate = interpolate
  )

  # colorRamp() refuses alpha whenever a space is specified, so interpolate the
  # alpha channel separately, linearly across evenly spaced stops.
  alphas <- grDevices::col2rgb(colors, alpha = TRUE)[4, ]
  alpha_at <- if (keep_alpha) {
    stops <- seq(0, 1, length.out = length(colors))
    function(t) stats::approx(stops, alphas, xout = t, rule = 2)$y
  } else {
    NULL
  }

  function(t) {
    t <- .clamp01(t)
    if (!length(t)) return(character(0))

    m <- f(t)
    # Lab interpolation and spline interpolation can both leave the sRGB
    # cube, so clamp. Round explicitly too: rgb() truncates, and the Lab
    # round trip returns e.g. 127.9999 for a channel that was exactly 128,
    # which would shift every stop one code value down.
    m[] <- round(pmin(255, pmax(0, m)))

    if (is.null(alpha_at)) {
      grDevices::rgb(m[, 1], m[, 2], m[, 3], maxColorValue = 255)
    } else {
      grDevices::rgb(m[, 1], m[, 2], m[, 3],
                     alpha = round(pmin(255, pmax(0, alpha_at(t)))),
                     maxColorValue = 255)
    }
  }
}

#' @noRd
.clamp01 <- function(t) {
  t <- as.numeric(t)
  pmin(1, pmax(0, t))
}

#' @noRd
.check_space <- function(space) {
  if (!is.character(space) || length(space) != 1) {
    stop("'space' must be \"Lab\" or \"sRGB\".", call. = FALSE)
  }
  # Accept common spellings; "rgb" is what grDevices calls sRGB.
  space <- switch(tolower(space),
                  lab = "Lab",
                  srgb = "sRGB",
                  rgb = "sRGB",
                  stop("'space' must be \"Lab\" or \"sRGB\".", call. = FALSE))
  space
}

#' @noRd
.check_interpolate <- function(interpolate) {
  if (!is.character(interpolate) || length(interpolate) != 1 ||
      !interpolate %in% c("linear", "spline")) {
    stop("'interpolate' must be \"linear\" or \"spline\".", call. = FALSE)
  }
  interpolate
}

#' Assemble a gradient evaluator
#'
#' Combines a weighting function (where colour change happens) with one or more
#' colour ramps (how colours mix) into a single function of relative position.
#'
#' @param colors Colour stops.
#' @param midpoints Relative positions of interior stops, or `NULL`.
#' @param weight_fun Function mapping \[0, 1\] to \[0, 1\], applied *within*
#'   each segment.
#' @param segmented When `FALSE`, `weight_fun` is applied once across the whole
#'   colour list instead of per segment (used by the custom mode).
#' @return A function mapping relative position `t` in \[0, 1\] to hex colours.
#' @noRd
.gradient_fun <- function(colors, midpoints = NULL, weight_fun,
                          space = "Lab", interpolate = "linear",
                          segmented = TRUE) {

  n <- length(colors)

  if (!segmented || n == 2) {
    ramp <- .make_ramp(colors, space, interpolate)
    return(function(t) ramp(weight_fun(.clamp01(t))))
  }

  stops <- c(0, midpoints, 1)
  ramps <- lapply(seq_len(n - 1), function(i) {
    .make_ramp(colors[i:(i + 1)], space, interpolate)
  })

  function(t) {
    t <- .clamp01(t)
    if (!length(t)) return(character(0))

    idx <- findInterval(t, stops, rightmost.closed = TRUE, all.inside = TRUE)
    out <- character(length(t))

    for (i in seq_len(n - 1)) {
      sel <- idx == i
      if (!any(sel)) next
      u <- (t[sel] - stops[i]) / (stops[i + 1] - stops[i])
      out[sel] <- ramps[[i]](weight_fun(u))
    }
    out
  }
}

#' Sample a gradient evaluator onto a data-unit grid
#'
#' @noRd
.gradient_df <- function(fun, resolution, low.value, high.value,
                         type = NULL, params = NULL) {

  t <- seq(0, 1, length.out = resolution + 1)

  df <- data.frame(
    value = seq(low.value, high.value, length.out = resolution + 1),
    color = fun(t),
    stringsAsFactors = FALSE
  )

  attr(df, "gradient_type") <- type
  attr(df, "gradient_params") <- params
  df
}
