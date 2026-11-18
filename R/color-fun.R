#' Turn a gradient specification into a colour function
#'
#' @description Returns a vectorised function mapping data values to hex
#'   colours. This is the interoperability layer: the same specification can
#'   then drive `ComplexHeatmap` (`col =`), base graphics, `image()`, `gt`,
#'   raster export, or anything else that accepts a value-to-colour function,
#'   not just `ggplot2` scales.
#'
#' @param params A `brouillet_gradient_params` object from [gradientParams()].
#' @param limits Numeric vector of length 2 giving the data range that maps onto
#'   the gradient.
#' @param na.color Colour returned for `NA`, `NaN` and non-finite input.
#' @return A function of one numeric vector, returning a character vector of hex
#'   colours the same length. Values outside `limits` get the clip colours when
#'   the specification defines them, and are otherwise clamped to the end
#'   colours.
#' @details Interface-compatible with `circlize::colorRamp2()`, so it can be
#'   passed straight to `ComplexHeatmap::Heatmap(col = ...)`.
#' @export
#' @examples
#' p <- gradientParams(colors = c("navy", "white", "firebrick"),
#'                      midpoints = 0.5, width = 0.3)
#' col_fun <- asColorFun(p, limits = c(-2, 2))
#'
#' col_fun(c(-2, -1, 0, 1, 2))
#' col_fun(c(-99, 99, NA))   # clamped, clamped, na.color
#'
#' \donttest{
#' # ComplexHeatmap::Heatmap(mat, col = col_fun)
#' image(volcano, col = col_fun(seq(90, 200, length.out = 64)))
#' }
asColorFun <- function(params, limits = c(0, 1), na.color = "grey95") {

  if (!inherits(params, "brouillet_gradient_params")) {
    stop("'params' must be a brouillet_gradient_params object from gradientParams().",
         call. = FALSE)
  }
  .check_limits(limits)
  .check_color(na.color, "na.color")
  .check_complete(params, "make a colour function")

  params <- .resolve_legacy(params, limits[1], limits[2])

  fun <- .params_gradient_fun(params)

  lo_clip <- params$low.clip.color
  hi_clip <- params$high.clip.color
  span <- diff(limits)

  # blend.clip fades the clip colour into the gradient end over the first 10% of
  # the range beyond the limits, matching what blendingClippingGradient() does
  # to the stop table. Without this, the same specification would render one way
  # through this function and another through the ggplot2 scales.
  blend <- isTRUE(params$blend.clip) && (!is.null(lo_clip) || !is.null(hi_clip))
  blend_frac <- 0.1

  structure(
    function(x) {
      x <- as.numeric(x)
      out <- rep(na.color, length(x))
      if (!length(x)) return(character(0))

      ok <- is.finite(x)
      if (!any(ok)) return(out)

      t <- (x[ok] - limits[1]) / span
      cols <- fun(t)   # fun() clamps t into [0, 1] itself

      if (!is.null(lo_clip)) {
        below <- t < 0
        if (any(below)) {
          cols[below] <- if (blend) {
            .make_ramp(c(fun(0), lo_clip), params$space,
                       params$interpolate)(pmin(1, -t[below] / blend_frac))
          } else {
            lo_clip
          }
        }
      }

      if (!is.null(hi_clip)) {
        above <- t > 1
        if (any(above)) {
          cols[above] <- if (blend) {
            .make_ramp(c(fun(1), hi_clip), params$space,
                       params$interpolate)(pmin(1, (t[above] - 1) / blend_frac))
          } else {
            hi_clip
          }
        }
      }

      out[ok] <- cols
      out
    },
    class = c("brouillet_color_fun", "function"),
    limits = limits,
    params = params
  )
}

#' @param x A `brouillet_color_fun`.
#' @param ... Ignored.
#' @rdname asColorFun
#' @export
print.brouillet_color_fun <- function(x, ...) {
  lim <- attr(x, "limits")
  cat("<brouillet colour function>\n")
  cat(" limits: ", lim[1], " to ", lim[2], "\n", sep = "")
  print(attr(x, "params"))
  invisible(x)
}

#' Build the evaluator for a gradient specification
#'
#' Shared by [buildGradient()], [asColorFun()] and the ggplot2 scales, so
#' all three are guaranteed to agree.
#'
#' @noRd
.params_gradient_fun <- function(params) {

  mode <- params$gradient_mode
  n <- length(params$colors)

  if (mode == "custom") {
    return(.gradient_fun(params$colors, NULL, params$mapping_fun,
                         params$space, params$interpolate, segmented = FALSE))
  }

  weight_fun <- if (!is.null(params$curve_fun)) {
    params$curve_fun
  } else {
    .resolve_weight_fun(
      shape = if (mode == "sigmoid") "sigmoid" else "linear",
      center = params$center,
      width = params$width,
      asymmetry = params$asymmetry,
      rescale = params$rescale
    )
  }

  if (isTRUE(params$legacy) && n == 3 && mode == "sigmoid") {
    mid_rel <- if (is.null(params$midpoints)) 0.5 else params$midpoints[1]
    left_n <- max(2, round(params$resolution * mid_rel))
    weight_fun <- list(.mirror_weight(weight_fun, shift = 1 / left_n), weight_fun)
  }

  midpoints <- if (n > 2) .default_midpoints(params$midpoints, n) else NULL

  .gradient_fun(params$colors, midpoints, weight_fun,
                params$space, params$interpolate, segmented = n > 2,
                reverse_segments = isTRUE(params$legacy) && n > 3)
}
