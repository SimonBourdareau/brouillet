## The curve decides *where* along the data range the colour change happens.
## Parameterised as center / width / asymmetry, all in relative units, so the
## numbers mean something without consulting a preset table.

# qlogis(0.95): a logistic reaches 95% at this many scale units from its centre.
.Z95 <- 2.944439

#' Monotone contrast curve
#'
#' @description An S-shaped mapping of \[0, 1\] onto \[0, 1\], used to control
#'   where a gradient's colour change is concentrated. Compressing the
#'   uninformative part of a data range and expanding a band of interest is the
#'   colour-scale equivalent of adjusting a contrast (gamma) slider.
#'
#' @param x Numeric vector of relative positions in \[0, 1\].
#' @param center Position of the 50\% point, in \[0, 1\]. This is where the
#'   colour change is fastest.
#' @param width Width of the transition: the span of `x` over which the curve
#'   goes from 5\% to 95\%. Small values give a sharp, threshold-like scale;
#'   `width = 1` is close to linear. Values above 1 flatten the curve further.
#' @param asymmetry Between -1 and 1. Shifts softness from the low side to the
#'   high side: positive values make the curve sharp below `center` and
#'   gradual above it (a long upper tail), negative values do the reverse.
#'   `0` is symmetric.
#' @param rescale When `TRUE` (default), the curve is linearly rescaled so that
#'   `x = 0` maps to exactly 0 and `x = 1` to exactly 1, guaranteeing that the
#'   end colours of a gradient are actually reached.
#' @return Numeric vector the same length as `x`, non-decreasing, in \[0, 1\].
#' @details
#' Below `center` the curve is a logistic of scale `width * (1 - asymmetry)`,
#' above it a logistic of scale `width * (1 + asymmetry)`; the two halves meet
#' at 0.5, so `center` is always the 50\% point and the 5-95\% span is always
#' approximately `width`.
#' @export
#' @examples
#' x <- seq(0, 1, length.out = 11)
#' round(contrastCurve(x), 3)                    # default, symmetric
#' round(contrastCurve(x, width = 0.1), 3)       # near-threshold
#' round(contrastCurve(x, asymmetry = 0.8), 3)   # long upper tail
#'
#' \donttest{
#' plot(seq(0, 1, 0.005), contrastCurve(seq(0, 1, 0.005), width = 0.25),
#'      type = "l", xlab = "data position", ylab = "colour position")
#' }
contrastCurve <- function(x, center = 0.5, width = 0.4, asymmetry = 0,
                           rescale = TRUE) {

  if (!is.numeric(x)) stop("'x' must be numeric.", call. = FALSE)
  .check_curve_args(center, width, asymmetry, rescale)

  k <- .curve_scales(width, asymmetry)

  raw <- function(z) {
    scale <- ifelse(z < center, k$lo, k$hi)
    1 / (1 + exp(-(z - center) / scale))
  }

  y <- raw(x)

  if (isTRUE(rescale)) {
    lo <- raw(0)
    hi <- raw(1)
    if (hi - lo > 1e-12) y <- (y - lo) / (hi - lo)
  }

  pmin(1, pmax(0, y))
}

#' Piecewise-linear counterpart of contrastCurve()
#'
#' Same vocabulary (center / width / asymmetry), straight lines instead of
#' logistics. `width = 1`, `asymmetry = 0` is the identity.
#'
#' @noRd
.linear_curve <- function(x, center = 0.5, width = 1, asymmetry = 0,
                          rescale = TRUE) {

  .check_curve_args(center, width, asymmetry, rescale)

  # Half-widths, so the ramp spans `width` in total.
  h_lo <- max(width * (1 - asymmetry), .Machine$double.eps)
  h_hi <- max(width * (1 + asymmetry), .Machine$double.eps)

  raw <- function(z) {
    slope <- ifelse(z < center, h_lo, h_hi)
    0.5 + (z - center) / slope
  }

  y <- pmin(1, pmax(0, raw(x)))

  if (isTRUE(rescale)) {
    lo <- min(1, max(0, raw(0)))
    hi <- min(1, max(0, raw(1)))
    if (hi - lo > 1e-12) y <- (y - lo) / (hi - lo)
  }

  pmin(1, pmax(0, y))
}

#' Scale parameters of the two logistic halves
#'
#' @noRd
.curve_scales <- function(width, asymmetry) {
  list(
    lo = max(width * (1 - asymmetry), .Machine$double.eps) / (2 * .Z95),
    hi = max(width * (1 + asymmetry), .Machine$double.eps) / (2 * .Z95)
  )
}

#' @noRd
.check_curve_args <- function(center, width, asymmetry, rescale = TRUE) {
  .check_unit_scalar(center, "center")
  if (!is.numeric(width) || length(width) != 1 || is.na(width) || width <= 0) {
    stop("'width' must be a single positive number.", call. = FALSE)
  }
  if (!is.numeric(asymmetry) || length(asymmetry) != 1 || is.na(asymmetry) ||
      asymmetry < -1 || asymmetry > 1) {
    stop("'asymmetry' must be a single number between -1 and 1.", call. = FALSE)
  }
  if (!is.logical(rescale) || length(rescale) != 1 || is.na(rescale)) {
    stop("'rescale' must be a single logical value.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Turn curve arguments into a weighting function
#'
#' @param shape `"sigmoid"` or `"linear"`.
#' @noRd
.resolve_weight_fun <- function(shape = c("sigmoid", "linear"),
                                center = 0.5, width = NULL, asymmetry = 0,
                                rescale = TRUE) {

  shape <- match.arg(shape)
  if (is.null(width)) width <- if (shape == "linear") 1 else 0.4

  .check_curve_args(center, width, asymmetry, rescale)

  if (shape == "linear") {
    function(u) .linear_curve(u, center, width, asymmetry, rescale)
  } else {
    function(u) contrastCurve(u, center, width, asymmetry, rescale)
  }
}

