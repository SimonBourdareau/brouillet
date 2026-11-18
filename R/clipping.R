#' Add hard out-of-range colours to a gradient
#'
#' @description Restricts a gradient to `[low.value, high.value]` and appends a
#'   single stop just below and just above that window, so values outside the
#'   window render in dedicated "clipped" colours.
#'
#' @param gradient A gradient data frame with `value` and `color` columns, or a
#'   specification from [gradientParams()] or [gradientTemplate()].
#' @param low.value,high.value Window to keep. Defaults to the gradient's own
#'   range.
#' @param low.clip.color,high.clip.color Colours for values below and above
#'   the window.
#' @return A gradient data frame with two extra rows.
#' @export
#' @examples
#' grad <- sigmoidGradient2("red", "blue")
#' clipped <- clippingGradient(grad, 0.2, 0.8, "grey80", "black")
#' head(clipped, 3)
clippingGradient <- function(gradient,
                              low.value = min(gradient$value),
                              high.value = max(gradient$value),
                              low.clip.color, high.clip.color) {

  if (.is_spec(gradient)) {
    return(.set_clip(gradient, low.clip.color, high.clip.color, blend = FALSE,
                     windowed = !missing(low.value) || !missing(high.value),
                     fun = "clippingGradient"))
  }

  .check_gradient_df(gradient)
  .check_range(low.value, high.value)

  if (missing(low.clip.color) || is.null(low.clip.color) ||
      missing(high.clip.color) || is.null(high.clip.color)) {
    stop("Both clip colours must be provided.", call. = FALSE)
  }
  .check_color(low.clip.color, "low.clip.color")
  .check_color(high.clip.color, "high.clip.color")

  keep <- gradient$value >= low.value & gradient$value <= high.value
  if (!any(keep)) {
    stop("No gradient values fall inside [low.value, high.value].", call. = FALSE)
  }
  out <- gradient[keep, , drop = FALSE]

  eps <- .eps_for(out$value)

  clipped <- rbind(
    data.frame(value = min(out$value) - eps, color = low.clip.color,
               stringsAsFactors = FALSE),
    out[, c("value", "color")],
    data.frame(value = max(out$value) + eps, color = high.clip.color,
               stringsAsFactors = FALSE)
  )
  rownames(clipped) <- NULL

  attr(clipped, "gradient_type") <-
    paste0("clip_", attr(gradient, "gradient_type"))
  attr(clipped, "gradient_params") <- attr(gradient, "gradient_params")

  clipped
}

#' Add blended out-of-range colours to a gradient
#'
#' @description Like [clippingGradient()], but the clip colours fade into the
#'   ends of the gradient over a short ramp instead of switching abruptly.
#'
#' @inheritParams clippingGradient
#' @param resolution Gradient resolution used to size the blend region; the
#'   blend uses `max(5, 10\% of resolution)` steps on each side.
#' @param .legacy Reproduce the pre-0.2 blend, which stopped short of the high
#'   clip colour. Set automatically for specifications from [gradient_params()];
#'   not intended to be passed directly.
#' @return A gradient data frame with extra rows at both ends.
#' @export
#' @examples
#' grad <- sigmoidGradient2("red", "blue")
#' blendingClippingGradient(grad, 0, 1, "grey80", "black")[1:3, ]
blendingClippingGradient <- function(gradient,
                                       low.value = min(gradient$value),
                                       high.value = max(gradient$value),
                                       low.clip.color, high.clip.color,
                                       resolution = 100, .legacy = FALSE) {

  if (.is_spec(gradient)) {
    return(.set_clip(gradient, low.clip.color, high.clip.color, blend = TRUE,
                     windowed = !missing(low.value) || !missing(high.value),
                     fun = "blendingClippingGradient"))
  }

  .check_gradient_df(gradient)
  .check_range(low.value, high.value)

  if (missing(low.clip.color) || is.null(low.clip.color) ||
      missing(high.clip.color) || is.null(high.clip.color)) {
    stop("Both clip colours must be provided.", call. = FALSE)
  }
  .check_color(low.clip.color, "low.clip.color")
  .check_color(high.clip.color, "high.clip.color")

  resolution <- .fix_resolution(resolution, min_res = 10)

  keep <- gradient$value >= low.value & gradient$value <= high.value
  if (!any(keep)) {
    stop("No gradient values fall inside [low.value, high.value].", call. = FALSE)
  }
  out <- gradient[keep, c("value", "color"), drop = FALSE]

  n_blend <- max(5, floor(resolution * 0.1))

  low_first <- out$color[1]
  high_last <- out$color[nrow(out)]

  low_blend <- colorRampPalette(c(low.clip.color, low_first))(n_blend + 2)[1:n_blend]
  high_blend <- if (isTRUE(.legacy)) {
    colorRampPalette(c(high_last, high.clip.color))(n_blend + 2)[1:n_blend]
  } else {
    colorRampPalette(c(high_last, high.clip.color))(n_blend + 2)[3:(n_blend + 2)]
  }

  step <- diff(range(out$value)) / max(1, nrow(out))
  if (!is.finite(step) || step <= 0) step <- .eps_for(out$value)

  min_val <- min(out$value)
  max_val <- max(out$value)

  low_vals <- seq(min_val - n_blend * step, min_val - step, length.out = n_blend)
  high_vals <- seq(max_val + step, max_val + n_blend * step, length.out = n_blend)

  clipped <- rbind(
    data.frame(value = low_vals, color = low_blend, stringsAsFactors = FALSE),
    out,
    data.frame(value = high_vals, color = high_blend, stringsAsFactors = FALSE)
  )
  rownames(clipped) <- NULL

  if (any(duplicated(clipped$value))) {
    warning("Duplicate values detected in blended gradient.", call. = FALSE)
  }

  attr(clipped, "gradient_type") <-
    paste0("blendclip_", attr(gradient, "gradient_type"))
  attr(clipped, "gradient_params") <- attr(gradient, "gradient_params")

  clipped
}

#' @noRd
.is_spec <- function(x) {
  inherits(x, c("brouillet_gradient_params", "brouillet_gradient_template"))
}

#' Set clip colours on a specification
#'
#' A specification has no data range, so the windowing arguments of the table
#' form do not apply to it.
#'
#' @noRd
.set_clip <- function(spec, low.clip.color, high.clip.color, blend, windowed,
                      fun) {

  if (windowed) {
    stop("'low.value' and 'high.value' trim a gradient table and do not apply ",
         "to a specification, which has no data range. Set the limits where the ",
         "gradient is drawn instead.", call. = FALSE)
  }

  lo <- if (missing(low.clip.color)) NULL else low.clip.color
  hi <- if (missing(high.clip.color)) NULL else high.clip.color

  if (is.null(lo) && is.null(hi)) {
    stop("Supply at least one of 'low.clip.color' and 'high.clip.color'.",
         call. = FALSE)
  }
  if (!is.null(lo)) .check_color(lo, "low.clip.color")
  if (!is.null(hi)) .check_color(hi, "high.clip.color")

  args <- list(low.clip.color = lo, high.clip.color = hi, blend.clip = blend)

  if (inherits(spec, "brouillet_gradient_template")) {
    keep <- unclass(spec)
    keep[names(args)] <- args
    return(do.call(gradientTemplate, keep))
  }

  do.call(updateGradient, c(list(params = spec), args))
}
