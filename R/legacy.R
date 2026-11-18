#' Double sigmoid curve
#'
#' @description Builds a weighting function from the product of two logistic
#'   curves: the mapping used by brouillet before 0.2, and a four-parameter
#'   family that [contrastCurve()] cannot express. A curve with distant
#'   inflexion points and small widths gives a staircase rather than a single
#'   transition.
#'
#' @param sigma1,sigma2 Widths of the two logistic factors, on an internal
#'   0-1000 grid. Smaller is sharper.
#' @param inflexpoint1,inflexpoint2 Centres of the two factors, as fractions in
#'   \[0, 1\], with `inflexpoint1 < inflexpoint2`.
#' @return A function mapping \[0, 1\] to \[0, 1\], suitable for the `curve_fun`
#'   argument of [gradientParams()].
#' @details Unlike [contrastCurve()] this is not rescaled, so it does not reach
#'   0 and 1 at the ends of the range. That is the behaviour the pre-0.2
#'   gradients had.
#' @export
#' @examples
#' f <- doubleSigmoid(200, 200, 0.3, 0.4)
#' round(f(c(0, 0.25, 0.5, 0.75, 1)), 3)
#'
#' gradientParams(c("navy", "firebrick"),
#'                curve_fun = doubleSigmoid(50, 50, 0.2, 0.8))
doubleSigmoid <- function(sigma1, sigma2, inflexpoint1 = 0.3, inflexpoint2 = 0.4) {

  .check_positive(sigma1, "sigma1")
  .check_positive(sigma2, "sigma2")
  .check_unit_scalar(inflexpoint1, "inflexpoint1")
  .check_unit_scalar(inflexpoint2, "inflexpoint2")

  if (inflexpoint1 >= inflexpoint2) {
    stop("'inflexpoint1' must be less than 'inflexpoint2'.", call. = FALSE)
  }

  m1 <- inflexpoint1 * 1000
  m2 <- inflexpoint2 * 1000

  function(x) {
    z <- .clamp01(x) * 1000
    (1 / (1 + exp(-(z - m1) / sigma1))) * (1 / (1 + exp(-(z - m2) / sigma2)))
  }
}

#' Steepness presets
#'
#' @description Named curve shapes carried over from the pre-0.2 API. Each row
#'   gives the original `sigma1`/`sigma2` pair, used by [gradient_params()] and
#'   [doubleSigmoid()], together with the `center`/`width`/`asymmetry` that
#'   reproduce it in the modern curve, used by
#'   `gradientParams(steepness_preset =)`.
#'
#' @return A data frame with one row per preset.
#' @details The modern columns were fitted to the 5\%, 50\% and 95\% points of
#'   the rescaled legacy curve, so a preset gives the same overall shape rather
#'   than identical colours. Use [gradient_params()] when you need the old
#'   rendering exactly.
#' @export
#' @examples
#' steepnessPresets()
#'
#' gradientParams(c("navy", "firebrick"), steepness_preset = "high")
steepnessPresets <- function() {
  data.frame(
    preset = names(.STEEPNESS_PRESETS),
    sigma1 = vapply(.STEEPNESS_PRESETS, `[[`, numeric(1), 1),
    sigma2 = vapply(.STEEPNESS_PRESETS, `[[`, numeric(1), 2),
    center = vapply(.MODERN_PRESETS, `[[`, numeric(1), 1),
    width = vapply(.MODERN_PRESETS, `[[`, numeric(1), 2),
    asymmetry = vapply(.MODERN_PRESETS, `[[`, numeric(1), 3),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

#' Modern curve settings fitted to each legacy preset
#'
#' Fitted to the 5/50/95\% points of the rescaled legacy curve.
#'
#' @noRd
.MODERN_PRESETS <- list(
  very_low         = c(0.56, 0.87, -0.10),
  lower            = c(0.55, 0.85, -0.08),
  low              = c(0.51, 0.76,  0.02),
  low_longtail     = c(0.53, 0.82, -0.02),
  medium           = c(0.45, 0.49,  0.12),
  medium_longtail  = c(0.53, 0.82, -0.02),
  high             = c(0.41, 0.26,  0.10),
  very_high        = c(0.40, 0.14,  0.04),
  extreme          = c(0.40, 0.03,  0.00),
  extreme_longtail = c(0.40, 0.49,  0.93)
)

#' @noRd
.modern_preset <- function(steepness_preset) {
  if (!is.character(steepness_preset) || length(steepness_preset) != 1 ||
      !steepness_preset %in% names(.MODERN_PRESETS)) {
    stop("Invalid steepness preset. Valid presets are: ",
         paste(names(.MODERN_PRESETS), collapse = ", "), call. = FALSE)
  }
  stats::setNames(as.list(.MODERN_PRESETS[[steepness_preset]]),
                  c("center", "width", "asymmetry"))
}

#' @noRd
.STEEPNESS_PRESETS <- list(
  very_low         = c(500, 500),
  lower            = c(350, 350),
  low              = c(200, 200),
  low_longtail     = c(500, 200),
  medium           = c(100, 100),
  medium_longtail  = c(500, 200),
  high             = c(50, 50),
  very_high        = c(25, 25),
  extreme          = c(5, 5),
  extreme_longtail = c(500, 5)
)

#' Resolve legacy sigmas, preset first
#'
#' The pre-0.2 code let a non-NULL `steepness_preset` override explicit sigmas,
#' and defaulted it to "low". So `sigma1 = 80, sigma2 = 80` had no effect unless
#' `steepness_preset = NULL` was also passed. Reproduced here deliberately:
#' changing it would alter how existing figures render.
#'
#' @noRd
.legacy_sigmas <- function(sigma1, sigma2, steepness_preset) {

  if (is.null(steepness_preset)) {
    if (is.null(sigma1) || is.null(sigma2)) {
      stop("Either 'steepness_preset' or both 'sigma1' and 'sigma2' must be provided.",
           call. = FALSE)
    }
    .check_positive(sigma1, "sigma1")
    .check_positive(sigma2, "sigma2")
    return(c(sigma1, sigma2))
  }
  if (!is.character(steepness_preset) || length(steepness_preset) != 1 ||
      !steepness_preset %in% names(.STEEPNESS_PRESETS)) {
    stop("Invalid steepness preset. Valid presets are: ",
         paste(names(.STEEPNESS_PRESETS), collapse = ", "), call. = FALSE)
  }

  .STEEPNESS_PRESETS[[steepness_preset]]
}

#' Create a gradient specification using the pre-0.2 arguments
#'
#' @description A drop-in replacement for the original `gradient_params()`:
#'   same arguments, same meanings, same rendered output. It returns a modern
#'   `brouillet_gradient_params` object, so the result works with every current
#'   function. New code should use [gradientParams()].
#'
#' @param colors Vector of colours, or `NULL` for a template.
#' @param midpoints Interior stop positions **in data units**, as before.
#'   Resolved against `limits` when the gradient is built.
#' @param sigma1,sigma2 Widths of the two logistic factors.
#' @param inflexpoint1,inflexpoint2 Transition points in \[0, 1\].
#' @param resolution Number of gradient steps.
#' @param steepness_preset One of [steepnessPresets()]; used when `sigma1` and
#'   `sigma2` are `NULL`.
#' @param low.clip.color,high.clip.color Colours for out-of-range values.
#' @param blend.clip Fade the clip colours in rather than switching.
#' @param gradient_mode `"sigmoid"`, `"linear"` or `"custom"`.
#' @param mapping_fun Function for `gradient_mode = "custom"`.
#' @return A `brouillet_gradient_params` object carrying legacy semantics.
#' @details Legacy specifications differ from [gradientParams()] defaults in
#'   four ways, all chosen so old figures regenerate unchanged: colours are
#'   interpolated in sRGB, the curve is not rescaled so the end colours are
#'   approached but not reached, `midpoints` are data-unit positions rather
#'   than fractions, and a three-colour gradient mirrors its lower half.
#' @export
#' @examples
#' p <- gradient_params(colors = c("navy", "white", "firebrick"),
#'                      midpoints = 0.5, steepness_preset = "medium")
#' p
#'
#' head(buildGradient(p, limits = c(0, 1)), 3)
gradient_params <- function(colors = NULL,
                            midpoints = NULL,
                            sigma1 = NULL,
                            sigma2 = NULL,
                            inflexpoint1 = 0.3,
                            inflexpoint2 = 0.4,
                            resolution = 100,
                            steepness_preset = "low",
                            low.clip.color = NULL,
                            high.clip.color = NULL,
                            blend.clip = FALSE,
                            gradient_mode = "sigmoid",
                            mapping_fun = NULL) {

  curve_fun <- NULL
  if (gradient_mode == "sigmoid") {
    sig <- .legacy_sigmas(sigma1, sigma2, steepness_preset)
    curve_fun <- doubleSigmoid(sig[1], sig[2], inflexpoint1, inflexpoint2)
  } else if (gradient_mode == "linear" && length(colors) == 2) {
    curve_fun <- .legacy_linear_curve(inflexpoint1, inflexpoint2)
  }

  params <- gradientParams(
    colors = colors,
    midpoints = NULL,
    resolution = resolution,
    space = "sRGB",
    rescale = FALSE,
    curve_fun = curve_fun,
    low.clip.color = low.clip.color,
    high.clip.color = high.clip.color,
    blend.clip = blend.clip,
    gradient_mode = gradient_mode,
    mapping_fun = mapping_fun
  )

  params$midpoints <- midpoints
  params$legacy <- TRUE

  structure(params, class = class(params))
}

#' Resolve legacy semantics once the data range is known
#'
#' Legacy `midpoints` are data-unit positions, so they cannot be turned into
#' fractions until the limits arrive. Three-colour legacy gradients also mirror
#' their lower half.
#'
#' @noRd
.resolve_legacy <- function(params, low.value, high.value) {

  if (!isTRUE(params$legacy)) return(params)

  if (!is.null(params$midpoints)) {
    rel <- (params$midpoints - low.value) / (high.value - low.value)
    if (any(rel <= 0 | rel >= 1)) {
      stop("Legacy 'midpoints' must lie strictly inside the limits: ",
           paste(format(params$midpoints), collapse = ", "), " not inside [",
           format(low.value), ", ", format(high.value), "].", call. = FALSE)
    }
    params$midpoints <- rel
  }

  params$legacy_resolved <- TRUE
  params
}

#' Piecewise-linear window used by the pre-0.2 two-colour linear gradient
#'
#' The original passed `inflexpoint1` and `inflexpoint2` straight through as the
#' start and end of the ramp, so a legacy linear gradient with the default
#' arguments ramps between 0.3 and 0.4 rather than across the whole range.
#' Multi-colour linear gradients ignored them.
#'
#' @noRd
.legacy_linear_curve <- function(inflexpoint1, inflexpoint2) {
  function(u) {
    u <- .clamp01(u)
    w <- numeric(length(u))
    w[u >= inflexpoint2] <- 1
    mid <- u > inflexpoint1 & u < inflexpoint2
    w[mid] <- (u[mid] - inflexpoint1) / (inflexpoint2 - inflexpoint1)
    w
  }
}
