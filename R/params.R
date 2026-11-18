#' Create a gradient specification
#'
#' @description Bundles every setting needed to build a gradient into a
#'   portable object, so one specification can drive several plots. Convert it
#'   with [asColorFun()] (any plotting system), [asScaleFillGradient()] or
#'   [asScaleColorGradient()] (`ggplot2`), or materialise it with
#'   [buildGradient()].
#'
#' @param colors Vector of at least two colours. Colour `i` runs to colour
#'   `i + 1` as the data value increases. May be `NULL`, which creates an
#'   incomplete specification: curve, clipping and interpolation settings are
#'   recorded, and the palette is supplied later with [updateGradient()]. Such
#'   an object cannot be built or plotted until it has colours.
#' @param midpoints Relative positions of the interior colour stops, strictly
#'   inside (0, 1) and strictly increasing, of length `length(colors) - 2`.
#'   Defaults to evenly spaced stops.
#' @param center,width,asymmetry Curve shape; see [contrastCurve()]. `width`
#'   defaults to 0.4 for `gradient_mode = "sigmoid"` and 1 for `"linear"`.
#' @param resolution Number of gradient steps (>= 2) used when sampling stops.
#' @param space Colour interpolation space: `"Lab"` (default, perceptual) or
#'   `"sRGB"`.
#' @param interpolate `"linear"` or `"spline"`.
#' @param rescale Force the end colours to be reached exactly; see
#'   [contrastCurve()]. Defaults to `TRUE` for `"sigmoid"` and `"custom"`,
#'   `FALSE` for `"linear"`.
#' @param low.clip.color,high.clip.color Colours for out-of-range values.
#'   `NULL` (default) disables clipping.
#' @param blend.clip Fade the clip colours into the gradient ends instead of
#'   switching abruptly.
#' @param gradient_mode `"sigmoid"`, `"linear"` or `"custom"`.
#' @param mapping_fun Monotonic function mapping \[0, 1\] to \[0, 1\], required
#'   when `gradient_mode = "custom"`.
#' @param curve_fun Function mapping \[0, 1\] to \[0, 1\] applied *within each
#'   segment*, replacing the built-in curve. Use it for shapes `center` /
#'   `width` / `asymmetry` cannot express, such as [doubleSigmoid()]. Unlike
#'   `mapping_fun` it does not change how colours are segmented.
#' @return An object of class `brouillet_gradient_params`.
#' @export
#' @examples
#' # Diverging, transition concentrated near the centre
#' p <- gradientParams(colors = c("navy", "white", "firebrick"),
#'                      midpoints = 0.5, width = 0.3)
#' p
#'
#' # Threshold-like scale with out-of-range colours
#' gradientParams(colors = c("grey90", "firebrick"),
#'                 center = 0.8, width = 0.1,
#'                 low.clip.color = "white", high.clip.color = "black")
gradientParams <- function(
    colors = NULL,
    midpoints = NULL,
    center = 0.5,
    width = NULL,
    asymmetry = 0,
    resolution = 100,
    space = "Lab",
    interpolate = "linear",
    rescale = NULL,
    low.clip.color = NULL,
    high.clip.color = NULL,
    blend.clip = FALSE,
    gradient_mode = "sigmoid",
    mapping_fun = NULL,
    curve_fun = NULL
) {

  # A NULL palette is allowed: the result is a template to be completed by
  # updateGradient(). Everything else is still validated now, so errors surface
  # at construction rather than at plot time.
  if (!is.null(colors)) .check_colors(colors, min_n = 2)
  n <- length(colors)

  valid_modes <- c("sigmoid", "linear", "custom")
  if (!is.character(gradient_mode) || length(gradient_mode) != 1 ||
      !gradient_mode %in% valid_modes) {
    stop("'gradient_mode' must be one of: ",
         paste(valid_modes, collapse = ", "), ".", call. = FALSE)
  }

  # Defaults depend on mode: a linear ramp already reaches its end colours by
  # clamping, so rescaling would only undo a deliberate width > 1 compression.
  if (is.null(width)) width <- if (gradient_mode == "linear") 1 else 0.4
  if (is.null(rescale)) rescale <- gradient_mode != "linear"

  if (is.null(colors)) {
    # Length depends on the eventual palette, so only check the values here.
    if (!is.null(midpoints)) .check_midpoint_values(midpoints)
  } else if (n > 2) {
    midpoints <- .default_midpoints(midpoints, n)
    .check_midpoints(midpoints, n)
  } else if (!is.null(midpoints)) {
    warning("'midpoints' is ignored for 2-colour gradients.", call. = FALSE)
    midpoints <- NULL
  }

  .check_curve_args(center, width, asymmetry, rescale)
  space <- .check_space(space)
  interpolate <- .check_interpolate(interpolate)

  if (!is.numeric(resolution) || length(resolution) != 1 ||
      is.na(resolution) || resolution < 2) {
    stop("'resolution' must be a single number >= 2.", call. = FALSE)
  }
  resolution <- as.integer(floor(resolution))

  if (!is.null(low.clip.color)) .check_color(low.clip.color, "low.clip.color")
  if (!is.null(high.clip.color)) .check_color(high.clip.color, "high.clip.color")

  if (!is.logical(blend.clip) || length(blend.clip) != 1 || is.na(blend.clip)) {
    stop("'blend.clip' must be a single logical value.", call. = FALSE)
  }

  if (gradient_mode == "custom") .check_mapping_fun(mapping_fun)

  if (!is.null(curve_fun)) {
    if (!is.function(curve_fun)) {
      stop("'curve_fun' must be a function.", call. = FALSE)
    }
    probe <- tryCatch(curve_fun(seq(0, 1, length.out = 5)),
                      error = function(e) {
                        stop("Error evaluating 'curve_fun': ", conditionMessage(e),
                             call. = FALSE)
                      })
    if (!is.numeric(probe) || length(probe) != 5 || any(!is.finite(probe))) {
      stop("'curve_fun' must return a finite numeric vector the same length as its input.",
           call. = FALSE)
    }
    if (any(probe < 0 | probe > 1)) {
      stop("'curve_fun' must return values in [0, 1].", call. = FALSE)
    }
  }

  # Validated eagerly so a bad curve fails here, not at plot time.
  .resolve_weight_fun(
    shape = if (gradient_mode == "linear") "linear" else "sigmoid",
    center = center, width = width, asymmetry = asymmetry, rescale = rescale
  )

  params <- list(
    colors = colors,
    midpoints = midpoints,
    center = center,
    width = width,
    asymmetry = asymmetry,
    resolution = resolution,
    space = space,
    interpolate = interpolate,
    rescale = rescale,
    low.clip.color = low.clip.color,
    high.clip.color = high.clip.color,
    blend.clip = blend.clip,
    gradient_mode = gradient_mode,
    mapping_fun = mapping_fun,
    curve_fun = curve_fun,
    created = Sys.time()
  )

  structure(params, class = "brouillet_gradient_params")
}

#' @param x A `brouillet_gradient_params` object.
#' @param ... Ignored.
#' @rdname gradientParams
#' @export
print.brouillet_gradient_params <- function(x, ...) {

  cat("<brouillet gradient", if (is.null(x$colors)) " template" else "", ">\n", sep = "")
  cat(" mode:       ", x$gradient_mode, "\n", sep = "")
  cat(" colors:     ",
      if (is.null(x$colors)) "<not set: complete with updateGradient()>"
      else paste(x$colors, collapse = " -> "), "\n", sep = "")
  cat(" space:      ", x$space, " (", x$interpolate, ")\n", sep = "")
  cat(" resolution: ", x$resolution, "\n", sep = "")

  if (!is.null(x$midpoints)) {
    cat(" midpoints:  ", paste(x$midpoints, collapse = ", "), "\n", sep = "")
  }

  if (isTRUE(x$legacy)) {
    cat(" curve:      legacy double sigmoid (pre-0.2 semantics)\n")
  } else if (!is.null(x$curve_fun)) {
    cat(" curve:      supplied curve_fun()\n")
  } else if (x$gradient_mode != "custom") {
      cat(" curve:      center ", x$center, ", width ", x$width,
          if (x$asymmetry != 0) paste0(", asymmetry ", x$asymmetry) else "",
          "\n", sep = "")
  }

  if (!is.null(x$low.clip.color) || !is.null(x$high.clip.color)) {
    cat(" clipping:   ",
        if (is.null(x$low.clip.color)) "none" else x$low.clip.color, " | ",
        if (is.null(x$high.clip.color)) "none" else x$high.clip.color,
        if (isTRUE(x$blend.clip)) " (blended)" else "", "\n", sep = "")
  }

  invisible(x)
}

#' Build a gradient data frame from a specification
#'
#' @description Materialises a [gradientParams()] specification over a data
#'   range, as a table of value/colour stops. Use [asColorFun()] instead when
#'   you want to colour arbitrary values.
#'
#' @param params A `brouillet_gradient_params` object.
#' @param limits Numeric vector of length 2 giving the data range.
#' @return A data frame with `value` and `color` columns.
#' @export
#' @examples
#' p <- gradientParams(colors = c("navy", "white", "firebrick"), midpoints = 0.5)
#' head(buildGradient(p, limits = c(-2, 2)))
buildGradient <- function(params, limits = c(0, 1)) {
  .check_limits(limits)
  .buildGradient(params, limits[1], limits[2])
}

#' @noRd
.buildGradient <- function(params, low.value, high.value) {

  if (!inherits(params, "brouillet_gradient_params")) {
    stop("'params' must be a brouillet_gradient_params object from gradientParams().",
         call. = FALSE)
  }
  .check_range(low.value, high.value)
  .check_complete(params)

  params <- .resolve_legacy(params, low.value, high.value)

  fun <- .params_gradient_fun(params)

  gradient_df <- .gradient_df(
    fun, params$resolution, low.value, high.value,
    type = paste0(params$gradient_mode, "_gradient"),
    params = params
  )

  if (!is.null(params$low.clip.color) || !is.null(params$high.clip.color)) {

    # Fall back to the gradient's own end colours when only one side is set.
    lo <- params$low.clip.color %||% gradient_df$color[1]
    hi <- params$high.clip.color %||% gradient_df$color[nrow(gradient_df)]

    gradient_df <- if (isTRUE(params$blend.clip)) {
      blendingClippingGradient(gradient_df, low.value, high.value, lo, hi,
                               resolution = params$resolution,
                               .legacy = isTRUE(params$legacy))
    } else {
      clippingGradient(gradient_df, low.value, high.value, lo, hi)
    }
  }

  gradient_df
}

#' Reject an incomplete specification
#'
#' @noRd
.check_complete <- function(params, what = "build this gradient") {
  if (is.null(params$colors)) {
    stop("This gradient specification has no colours, so it is not possible to ",
         what, ". Supply a palette with updateGradient(params, colors = ...) ",
         "or gradientParams(colors = ...).", call. = FALSE)
  }
  invisible(TRUE)
}

#' Modify a gradient specification
#'
#' @description Returns a new specification with the named fields replaced,
#'   revalidated as if it had been built by [gradientParams()] from scratch.
#'   This is the supported way to complete a colourless template, or to adjust
#'   one setting without restating the rest.
#'
#' @param params A `brouillet_gradient_params` object.
#' @param ... Named fields to replace; any argument of [gradientParams()].
#'   Pass `NULL` to reset a field to its default.
#' @return A new `brouillet_gradient_params` object.
#' @details When `colors` changes length and `midpoints` is not supplied, stale
#'   midpoints are dropped (with a warning) and the evenly spaced default is
#'   used, rather than failing on a length mismatch.
#' @export
#' @examples
#' # A reusable template: curve and clipping now, colours later
#' tmpl <- gradientParams(colors = NULL, width = 0.3, high.clip.color = "black")
#' tmpl
#'
#' p <- updateGradient(tmpl, colors = c("navy", "white", "firebrick"))
#' p
#'
#' # Adjust one field
#' updateGradient(p, width = 0.1)$width
updateGradient <- function(params, ...) {

  if (!inherits(params, "brouillet_gradient_params")) {
    stop("'params' must be a brouillet_gradient_params object from gradientParams().",
         call. = FALSE)
  }

  fields <- c("colors", "midpoints", "center", "width", "asymmetry", "resolution",
              "space", "interpolate", "rescale", "low.clip.color", "high.clip.color",
              "blend.clip", "gradient_mode", "mapping_fun", "curve_fun"
              )

  new <- list(...)
  if (length(new) && (is.null(names(new)) || any(names(new) == ""))) {
    stop("All arguments to updateGradient() must be named.", call. = FALSE)
  }

  unknown <- setdiff(names(new), fields)
  if (length(unknown)) {
    stop("Unknown gradient field(s): ", paste(unknown, collapse = ", "),
         ". Valid fields are: ", paste(fields, collapse = ", "), ".", call. = FALSE)
  }

  args <- params[fields]
  # Assigning NULL drops the element, which is what resetting a field means:
  # gradientParams() then applies its own default.
  for (nm in names(new)) args[[nm]] <- new[[nm]]

  # Colours changed shape and the caller did not say where the interior stops go.
  if ("colors" %in% names(new) && !("midpoints" %in% names(new)) &&
      !is.null(args$midpoints) && !is.null(args$colors) &&
      length(args$midpoints) != length(args$colors) - 2) {
    warning("'midpoints' does not fit the new palette and was dropped; ",
            "using evenly spaced stops.", call. = FALSE)
    args$midpoints <- NULL
  }

  legacy_midpoints <- NULL
  if (isTRUE(params$legacy)) {
    legacy_midpoints <- args$midpoints
    args$midpoints <- NULL
  }

  out <- do.call(gradientParams, args)

  if (isTRUE(params$legacy)) {
    args$midpoints <- legacy_midpoints
    out$legacy <- TRUE
    out$midpoints <- if ("midpoints" %in% names(new)) new$midpoints else params$midpoints
  }

  out
}

#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' @noRd
.check_limits <- function(limits) {
  if (!is.numeric(limits) || length(limits) != 2 || any(!is.finite(limits)) ||
      limits[1] >= limits[2]) {
    stop("'limits' must be a numeric vector of length 2 with limits[1] < limits[2].",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Reverse a gradient specification
#'
#' @description Mirrors a specification: colour order, midpoints, curve centre,
#'   curve asymmetry and clip colours are all flipped, so the reversed gradient
#'   is the mirror image of the original rather than just its colours in
#'   reverse.
#'
#' @param params A `brouillet_gradient_params` object.
#' @return A new `brouillet_gradient_params` object.
#' @export
#' @examples
#' p <- gradientParams(colors = c("navy", "white", "firebrick"),
#'                      midpoints = 0.4, center = 0.3, asymmetry = 0.5)
#' reverseGradient(p)
reverseGradient <- function(params) {

  if (!inherits(params, "brouillet_gradient_params")) {
    stop("'params' must be a brouillet_gradient_params object from gradientParams().",
         call. = FALSE)
  }

  new_params <- params
  new_params$colors <- rev(params$colors)

  if (!is.null(params$midpoints)) {
    new_params$midpoints <- 1 - rev(params$midpoints)
  }

  new_params$center <- 1 - params$center
  new_params$asymmetry <- -params$asymmetry

  new_params$low.clip.color <- params$high.clip.color
  new_params$high.clip.color <- params$low.clip.color

  if (params$gradient_mode == "custom" && is.function(params$mapping_fun)) {
    f <- params$mapping_fun
    new_params$mapping_fun <- function(x) 1 - f(1 - x)
  }

  structure(new_params, class = class(params))
}
