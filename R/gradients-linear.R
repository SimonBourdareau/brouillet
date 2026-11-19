#' Two-colour linear gradient
#'
#' @description Piecewise-linear ramp: constant `color1`, a straight-line blend
#'   of total span `width` centred on `center`, then constant `color2`. The
#'   default (`center = 0.5`, `width = 1`) is an ordinary linear gradient.
#'
#' @inheritParams sigmoidGradient2
#' @param color1 Colour at the low end.
#' @param color2 Colour at the high end.
#' @param center Position of the 50\% point, in \[0, 1\].
#' @param width Span of the ramp, in relative units. Values below 1 leave flat
#'   regions at both ends.
#' @param asymmetry Between -1 and 1; makes the ramp steeper on one side of
#'   `center` than the other.
#' @return A data frame with columns `value` and `color`.
#' @export
#' @examples
#' linearGradient2("navy", "firebrick")[1:3, ]
#'
#' # Flat below 0.25 and above 0.75
#' linearGradient2("navy", "firebrick", width = 0.5)[1:3, ]
linearGradient2 <- function(color1, color2,
                             center = 0.5, width = 1, asymmetry = 0,
                             resolution = 100,
                             low.value = 0, high.value = 1,
                             space = "Lab", interpolate = "linear",
                             rescale = FALSE) {

  .check_color(color1, "color1")
  .check_color(color2, "color2")
  .check_range(low.value, high.value)
  resolution <- .fix_resolution(resolution, min_res = 2)

  weight_fun <- .resolve_weight_fun("linear", center, width, asymmetry, rescale)

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
               "linearGradient2", params)
}

#' Multi-colour linear gradient
#'
#' @description Chains linear ramps between colour stops.
#'
#' @inheritParams linearGradient2
#' @param colors Vector of at least two colours. Segment `i` runs from
#'   `colors[i]` to `colors[i + 1]` as `value` increases.
#' @param midpoints Relative positions of the interior stops, strictly inside
#'   (0, 1), strictly increasing, of length `length(colors) - 2`. Defaults to
#'   evenly spaced stops.
#' @return A data frame with columns `value` and `color`.
#' @export
#' @examples
#' linearGradientn(c("navy", "white", "firebrick"), 0.5)[1:3, ]
linearGradientn <- function(colors, midpoints = NULL,
                             center = 0.5, width = 1, asymmetry = 0,
                             resolution = 100,
                             low.value = 0, high.value = 1,
                             space = "Lab", interpolate = "linear",
                             rescale = FALSE) {

  .check_colors(colors, min_n = 2)
  .check_range(low.value, high.value)
  resolution <- .fix_resolution(resolution, min_res = 2)

  n <- length(colors)
  if (n > 2) {
    midpoints <- .default_midpoints(midpoints, n)
    .check_midpoints(midpoints, n)
  } else {
    midpoints <- NULL
  }

  weight_fun <- .resolve_weight_fun("linear", center, width, asymmetry, rescale)

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
               "linearGradientn", params)
}

#' Gradient from a custom mapping function
#'
#' @description Maps relative position through a user-supplied monotonic
#'   function, then samples a single ramp built from all of `colors`.
#'
#' @inheritParams sigmoidGradient2
#' @param colors Vector of at least two colours.
#' @param mapping.fun A vectorised, non-decreasing function mapping \[0, 1\] to
#'   \[0, 1\].
#' @return A data frame with columns `value` and `color`.
#' @export
#' @examples
#' # Slow start, fast finish
#' customGradientFun(c("navy", "firebrick"), function(x) x^2)[1:3, ]
customGradientFun <- function(colors, mapping.fun,
                                resolution = 100,
                                low.value = 0, high.value = 1,
                                space = "Lab", interpolate = "linear") {

  .check_colors(colors, min_n = 2)
  .check_range(low.value, high.value)
  resolution <- .fix_resolution(resolution, min_res = 2)
  .check_mapping_fun(mapping.fun)

  # Applied across the whole colour list, not per segment.
  fun <- .gradient_fun(colors, NULL, mapping.fun, space, interpolate,
                       segmented = FALSE)

  params <- list(
    colors = colors, mapping.fun = mapping.fun,
    resolution = resolution,
    space = space, interpolate = interpolate,
    low.value = low.value, high.value = high.value,
    function_call = match.call()
  )

  .gradient_df(fun, resolution, low.value, high.value,
               "custom_gradient", params)
}

#' Validate a custom mapping function
#'
#' @noRd
.check_mapping_fun <- function(mapping.fun) {

  if (!is.function(mapping.fun)) {
    stop("'mapping.fun' must be a function.", call. = FALSE)
  }

  test_x <- seq(0, 1, length.out = 5)
  test_y <- tryCatch(mapping.fun(test_x),
                     error = function(e) {
                       stop("Error evaluating 'mapping.fun': ", conditionMessage(e),
                            call. = FALSE)
                     })

  if (!is.numeric(test_y) || length(test_y) != length(test_x)) {
    stop("'mapping.fun' must return a numeric vector the same length as its input.",
         call. = FALSE)
  }
  if (any(!is.finite(test_y))) {
    stop("'mapping.fun' returned non-finite values.", call. = FALSE)
  }
  if (any(test_y < 0 | test_y > 1)) {
    stop("'mapping.fun' must return values in [0, 1].", call. = FALSE)
  }
  if (is.unsorted(test_y)) {
    stop("'mapping.fun' must be monotonic (non-decreasing).", call. = FALSE)
  }

  invisible(TRUE)
}
