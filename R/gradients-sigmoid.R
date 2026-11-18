#' Two-colour contrast gradient
#'
#' @description Builds a gradient between two colours where the colour change
#'   is concentrated around `center`, following [contrastCurve()].
#'
#' @param color1 Colour at the low end.
#' @param color2 Colour at the high end.
#' @param center,width,asymmetry Curve shape; see [contrastCurve()].
#' @param resolution Number of gradient steps (>= 2). The returned data frame
#'   has `resolution + 1` rows.
#' @param low.value,high.value Data-unit bounds of the gradient.
#' @param space Colour interpolation space: `"Lab"` (default, perceptual) or
#'   `"sRGB"` (the pre-0.2.0 behaviour).
#' @param interpolate `"linear"` or `"spline"` interpolation between stops.
#' @param rescale Force the end colours to be reached exactly; see
#'   [contrastCurve()].
#' @return A data frame with columns `value` and `color`, carrying
#'   `gradientParams` and `gradient_type` attributes.
#' @export
#' @examples
#' g <- sigmoidGradient2("navy", "firebrick")
#' head(g)
#'
#' # Sharpen the transition and move it up the range
#' sigmoidGradient2("navy", "firebrick", center = 0.7, width = 0.15)[1:3, ]
sigmoidGradient2 <- function(color1, color2,
                              center = 0.5, width = 0.4, asymmetry = 0,
                              resolution = 100,
                              low.value = 0, high.value = 1,
                              space = "Lab", interpolate = "linear",
                              rescale = TRUE) {

  .check_color(color1, "color1")
  .check_color(color2, "color2")
  .check_range(low.value, high.value)
  resolution <- .fix_resolution(resolution, min_res = 2)

  weight_fun <- .resolve_weight_fun("sigmoid", center, width, asymmetry, rescale)

  fun <- .gradient_fun(c(color1, color2), NULL, weight_fun, space, interpolate)

  params <- list(
    color1 = color1, color2 = color2,
    center = center, width = width, asymmetry = asymmetry,
    resolution = resolution, rescale = rescale,
    space = space, interpolate = interpolate,
    low.value = low.value, high.value = high.value,
    function_call = match.call()
  )

  .gradient_df(fun, resolution, low.value, high.value,
               "sigmoidGradient2", params)
}

#' Three-colour contrast gradient
#'
#' @description Two contrast ramps meeting at a central colour, the usual shape
#'   for diverging scales.
#'
#' @inheritParams sigmoidGradient2
#' @param midpoint.color Colour at the centre of the gradient.
#' @param midpoint.value Data-unit position of the central colour; must lie
#'   strictly between `low.value` and `high.value`.
#' @return A data frame with columns `value` and `color`.
#' @details The curve is applied within each half, so with the default
#'   `center = 0.5` and `asymmetry = 0` the result is symmetric about
#'   `midpoint.value`.
#' @export
#' @examples
#' g <- sigmoidGradient3("navy", "firebrick", "white", width = 0.3)
#' head(g)
sigmoidGradient3 <- function(color1, color2, midpoint.color,
                              center = 0.5, width = 0.4, asymmetry = 0,
                              resolution = 100,
                              low.value = 0, midpoint.value = 0.5, high.value = 1,
                              space = "Lab", interpolate = "linear",
                              rescale = TRUE) {

  .check_color(color1, "color1")
  .check_color(color2, "color2")
  .check_color(midpoint.color, "midpoint.color")
  .check_range(low.value, high.value)
  resolution <- .fix_resolution(resolution, min_res = 4)

  if (!is.numeric(midpoint.value) || length(midpoint.value) != 1 ||
      !is.finite(midpoint.value) ||
      midpoint.value <= low.value || midpoint.value >= high.value) {
    stop("Values must satisfy: low.value < midpoint.value < high.value.", call. = FALSE)
  }

  weight_fun <- .resolve_weight_fun("sigmoid", center, width, asymmetry, rescale)

  midpoint_rel <- (midpoint.value - low.value) / (high.value - low.value)

  fun <- .gradient_fun(c(color1, midpoint.color, color2), midpoint_rel,
                       weight_fun, space, interpolate)

  params <- list(
    color1 = color1, color2 = color2, midpoint.color = midpoint.color,
    center = center, width = width, asymmetry = asymmetry,
    resolution = resolution, rescale = rescale,
    space = space, interpolate = interpolate,
    low.value = low.value, midpoint.value = midpoint.value, high.value = high.value,
    function_call = match.call()
  )

  .gradient_df(fun, resolution, low.value, high.value,
               "sigmoidGradient3", params)
}

#' Multi-colour contrast gradient
#'
#' @description Chains contrast ramps between three or more colour stops.
#'
#' @inheritParams sigmoidGradient2
#' @param colors Vector of at least three colours. Segment `i` runs from
#'   `colors[i]` to `colors[i + 1]` as `value` increases.
#' @param midpoints Relative positions of the interior stops, strictly inside
#'   (0, 1), strictly increasing, of length `length(colors) - 2`. Defaults to
#'   evenly spaced stops.
#' @return A data frame with columns `value` and `color`.
#' @export
#' @examples
#' g <- sigmoidGradientn(c("navy", "white", "firebrick", "black"), c(0.4, 0.8))
#' head(g)
sigmoidGradientn <- function(colors, midpoints = NULL,
                              center = 0.5, width = 0.4, asymmetry = 0,
                              resolution = 100,
                              low.value = 0, high.value = 1,
                              space = "Lab", interpolate = "linear",
                              rescale = TRUE) {

  .check_colors(colors, min_n = 3)
  .check_range(low.value, high.value)
  resolution <- .fix_resolution(resolution, min_res = 4)

  n <- length(colors)
  midpoints <- .default_midpoints(midpoints, n)
  .check_midpoints(midpoints, n)

  weight_fun <- .resolve_weight_fun("sigmoid", center, width, asymmetry, rescale)

  fun <- .gradient_fun(colors, midpoints, weight_fun, space, interpolate)

  params <- list(
    colors = colors, midpoints = midpoints,
    center = center, width = width, asymmetry = asymmetry,
    resolution = resolution, rescale = rescale,
    space = space, interpolate = interpolate,
    low.value = low.value, high.value = high.value,
    function_call = match.call()
  )

  .gradient_df(fun, resolution, low.value, high.value,
               "sigmoidGradientn", params)
}

#' Evenly spaced interior stops when midpoints are not supplied
#'
#' @noRd
.default_midpoints <- function(midpoints, n_colors) {
  if (!is.null(midpoints) || n_colors <= 2) return(midpoints)
  seq_len(n_colors - 2) / (n_colors - 1)
}
