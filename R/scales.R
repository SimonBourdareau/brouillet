#' Assemble colours/values/limits for a ggplot2 gradient scale
#'
#' Adds visible bands for the clip colours when requested, keeps breakpoints
#' strictly increasing (duplicated breakpoints break interpolation), and
#' returns limits that match the value range exactly.
#'
#' @noRd
.scale_pieces <- function(params, limits, show.clip, clip.width) {

  gradient_df <- .buildGradient(params, limits[1], limits[2])

  values <- gradient_df$value
  colours <- gradient_df$color

  has_clip <- !is.null(params$low.clip.color) || !is.null(params$high.clip.color)

  # With blend.clip the stop table already runs past the limits and ends on the
  # clip colours. Injecting hard clip bands on top of that drops a pure clip
  # colour into the middle of the blend, which draws as a hard line.
  if (isTRUE(show.clip) && has_clip && !isTRUE(params$blend.clip)) {

    w <- diff(limits) * clip.width
    eps <- max(w * 1e-3, .Machine$double.eps)

    lo <- params$low.clip.color %||% colours[1]
    hi <- params$high.clip.color %||% colours[length(colours)]

    values  <- c(limits[1] - w, min(values) - eps, values,
                 max(values) + eps, limits[2] + w)
    colours <- c(lo, lo, colours, hi, hi)
  }

  ord <- order(values)
  values <- values[ord]
  colours <- colours[ord]

  dup <- duplicated(values)
  values <- values[!dup]
  colours <- colours[!dup]

  list(values = values, colours = colours, limits = range(values))
}

#' Use a gradient specification as a ggplot2 fill scale
#'
#' @param params A `brouillet_gradient_params` object from [gradientParams()].
#' @param limits Numeric vector of length 2 giving the data range.
#' @param na.value Colour for missing values.
#' @param guide Guide passed to the scale.
#' @param show.clip Show the clip colours as bands beyond `limits`.
#' @param clip.width Width of each clip band, as a proportion of the range.
#' @param ... Passed to [ggplot2::scale_fill_gradientn()].
#' @return A `ggplot2` scale object.
#' @details When `show.clip = TRUE` and clip colours are set, the scale limits
#'   are widened by `clip.width` on each side so the clipped bands are visible
#'   in the colourbar.
#' @export
#' @examples
#' \donttest{
#' library(ggplot2)
#'
#' params <- gradientParams(colors = c("navy", "white", "red"), midpoints = 0.5)
#'
#' df <- expand.grid(x = 1:10, y = 1:10)
#' df$z <- runif(100)
#'
#' ggplot(df, aes(x, y, fill = z)) +
#'   geom_tile() +
#'   asScaleFillGradient(params)
#' }
asScaleFillGradient <- function(params,
                                   limits = c(0, 1),
                                   na.value = "grey95",
                                   guide = guide_colourbar(),
                                   show.clip = TRUE,
                                   clip.width = 0.02,
                                   ...) {

  pieces <- .validate_scale_args(params, limits, na.value, show.clip, clip.width)

  do.call(scale_fill_gradientn,
          .scale_args(pieces, na.value, guide, list(...)))
}

#' Use a gradient specification as a ggplot2 colour scale
#'
#' @inheritParams asScaleFillGradient
#' @param ... Passed to [ggplot2::scale_color_gradientn()].
#' @return A `ggplot2` scale object.
#' @export
#' @examples
#' \donttest{
#' library(ggplot2)
#'
#' params <- gradientParams(colors = c("navy", "white", "red"), midpoints = 0.5)
#'
#' ggplot(mtcars, aes(wt, mpg, color = qsec)) +
#'   geom_point(size = 3) +
#'   asScaleColorGradient(params, limits = range(mtcars$qsec))
#' }
asScaleColorGradient <- function(params,
                                    limits = c(0, 1),
                                    na.value = "grey95",
                                    guide = guide_colourbar(),
                                    show.clip = TRUE,
                                    clip.width = 0.02,
                                    ...) {

  pieces <- .validate_scale_args(params, limits, na.value, show.clip, clip.width)

  do.call(scale_color_gradientn,
          .scale_args(pieces, na.value, guide, list(...)))
}

#' Assemble the argument list for a ggplot2 gradient scale
#'
#' Out-of-range values are squished into the clip bands rather than falling
#' through to `na.value`: without this, only a sliver of width `clip.width`
#' beyond `limits` gets the clip colour and anything further out renders as
#' missing. A user-supplied `oob` still wins.
#'
#' @noRd
.scale_args <- function(pieces, na.value, guide, dots) {

  args <- list(
    colours = pieces$colours,
    values = scales::rescale(pieces$values),
    limits = pieces$limits,
    na.value = na.value,
    guide = guide
  )

  if (is.null(dots$oob)) dots$oob <- scales::oob_squish

  c(args, dots)
}

#' @noRd
.validate_scale_args <- function(params, limits, na.value, show.clip, clip.width) {

  if (!inherits(params, "brouillet_gradient_params")) {
    stop("'params' must be a brouillet_gradient_params object from gradientParams().",
         call. = FALSE)
  }
  .check_limits(limits)
  .check_color(na.value, "na.value")
  .check_complete(params, "build a ggplot2 scale")

  if (!is.logical(show.clip) || length(show.clip) != 1 || is.na(show.clip)) {
    stop("'show.clip' must be a single logical value.", call. = FALSE)
  }
  if (!is.numeric(clip.width) || length(clip.width) != 1 ||
      is.na(clip.width) || clip.width <= 0 || clip.width > 1) {
    stop("'clip.width' must be a single number in (0, 1].", call. = FALSE)
  }

  .scale_pieces(params, limits, show.clip, clip.width)
}

#' Preview a gradient
#'
#' @description Renders a gradient specification as a single horizontal band.
#'
#' @param params A `brouillet_gradient_params` object.
#' @param limits Numeric vector of length 2 giving the range to preview.
#' @param n_ticks Number of x-axis ticks (>= 2).
#' @param clip.pad When the specification sets clip colours, how far beyond
#'   `limits` to draw, as a proportion of the range, so the clipped regions are
#'   visible. Ignored when there is no clipping.
#' @return A `ggplot` object.
#' @export
#' @examples
#' \donttest{
#' params <- gradientParams(colors = c("blue", "white", "red"), midpoints = 0.5)
#' previewGradient(params)
#' }
previewGradient <- function(params, limits = c(0, 1), n_ticks = 5,
                            clip.pad = 0.08) {

  if (!inherits(params, "brouillet_gradient_params")) {
    stop("'params' must be a brouillet_gradient_params object from gradientParams().",
         call. = FALSE)
  }
  .check_limits(limits)

  if (!is.numeric(n_ticks) || length(n_ticks) != 1 || is.na(n_ticks) || n_ticks < 2) {
    warning("'n_ticks' must be >= 2. Using default value of 5.", call. = FALSE)
    n_ticks <- 5
  }
  n_ticks <- floor(n_ticks)

  if (!is.numeric(clip.pad) || length(clip.pad) != 1 || is.na(clip.pad) ||
      clip.pad < 0 || clip.pad >= 0.5) {
    stop("'clip.pad' must be a single number in [0, 0.5).", call. = FALSE)
  }

  # A preview that stops at the limits can never show the clip colours, since
  # no value is out of range. Extend the drawn range when they are set.
  pad <- if (hasClipColors(params)) diff(limits) * clip.pad else 0

  df <- data.frame(
    value = seq(limits[1] - pad, limits[2] + pad, length.out = 500),
    y = 1
  )

  ggplot(df, aes(x = .data$value, y = .data$y, fill = .data$value)) +
    geom_tile(height = 1) +
    asScaleFillGradient(params, limits = limits,
                           show.clip = TRUE, clip.width = 0.02) +
    scale_x_continuous(
      breaks = seq(limits[1], limits[2], length.out = n_ticks),
      expand = c(0, 0)
    ) +
    scale_y_continuous(NULL, breaks = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    )
}
