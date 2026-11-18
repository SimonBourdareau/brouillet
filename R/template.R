## Support for host functions that own the palette but let the caller shape the
## curve: PlotInteractionMap() takes low/mid/high colours of its own, so a
## caller wanting a sigmoid needs to specify the curve *without* colours.

#' Create a gradient template (curve settings without colours)
#'
#' @description Captures everything [gradientParams()] takes except the
#'   colours, for use with functions that supply their own palette. Combine with
#'   [withColors()] to get a usable specification.
#'
#' @inheritParams gradientParams
#' @return An object of class `brouillet_gradient_template`.
#' @details All arguments are validated immediately against a placeholder
#'   palette, so a bad curve or mode fails here rather than at plot time.
#' @export
#' @examples
#' tpl <- gradientTemplate(center = 0.4, width = 0.25)
#' tpl
#'
#' # A host function fills in its own colours
#' p <- withColors(tpl, c("blue", "white", "red"), midpoints = 0.5)
#' head(buildGradient(p))
gradientTemplate <- function(center = 0.5,
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
                              curve_fun = NULL,
                              steepness_preset = NULL) {

  args <- list(
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
    steepness_preset = steepness_preset
  )

  # Validate now, with a throwaway palette, so errors surface at the call site.
  do.call(gradientParams, c(list(colors = c("black", "white")), args))

  structure(args, class = "brouillet_gradient_template")
}

#' @param x A `brouillet_gradient_template`.
#' @param ... Ignored.
#' @rdname gradientTemplate
#' @export
print.brouillet_gradient_template <- function(x, ...) {
  cat("<brouillet gradient template>  (no colours yet)\n")
  cat(" mode:       ", x$gradient_mode, "\n", sep = "")
  cat(" space:      ", x$space, " (", x$interpolate, ")\n", sep = "")

  if (x$gradient_mode != "custom") {
      cat(" curve:      center ", x$center,
          ", width ", if (is.null(x$width)) "default" else x$width,
          if (x$asymmetry != 0) paste0(", asymmetry ", x$asymmetry) else "",
          "\n", sep = "")
  }

  if (!is.null(x$low.clip.color) || !is.null(x$high.clip.color)) {
    cat(" clipping:   ",
        if (is.null(x$low.clip.color)) "none" else x$low.clip.color, " | ",
        if (is.null(x$high.clip.color)) "none" else x$high.clip.color, "\n", sep = "")
  }

  cat(" complete it with withColors(x, colors)\n")
  invisible(x)
}

#' Attach colours to a template or replace those of a specification
#'
#' @description The bridge between a caller-supplied curve and a host-supplied
#'   palette. Also useful for swapping the palette of an existing specification
#'   while keeping its curve, clipping and interpolation settings.
#'
#' @param x A `brouillet_gradient_template` or `brouillet_gradient_params`.
#' @param colors Vector of at least two colours.
#' @param midpoints Relative positions of the interior stops, in (0, 1). `NULL`
#'   uses evenly spaced stops. Data-unit positions must be converted first with
#'   [relativeMidpoint()].
#' @return A `brouillet_gradient_params` object.
#' @export
#' @examples
#' tpl <- gradientTemplate(width = 0.3)
#' withColors(tpl, c("blue", "white", "red"), midpoints = 0.5)
#'
#' # Same curve, different palette
#' p <- gradientParams(c("navy", "white", "firebrick"), width = 0.2)
#' withColors(p, c("black", "grey", "gold"))
withColors <- function(x, colors, midpoints = NULL) {

  if (inherits(x, "brouillet_gradient_template")) {
    args <- unclass(x)
  } else if (inherits(x, "brouillet_gradient_params")) {
    args <- .params_args(x)
  } else {
    stop("'x' must be a gradient template or a brouillet_gradient_params object.",
         call. = FALSE)
  }

  out <- do.call(gradientParams, c(list(colors = colors,
                                        midpoints = if (isTRUE(x$legacy)) NULL else midpoints),
                                  args))

  if (isTRUE(x$legacy)) {
    out$legacy <- TRUE
    out$midpoints <- if (is.null(midpoints)) x$midpoints else midpoints
  }

  out
}

#' Constructor arguments of a specification, minus colours
#'
#' @noRd
.params_args <- function(params) {
  keep <- c("center", "width", "asymmetry", "resolution", "space", "interpolate",
            "rescale", "low.clip.color", "high.clip.color", "blend.clip",
            "gradient_mode", "mapping_fun", "curve_fun", "steepness_preset")
  unclass(params)[keep]
}

#' Convert a data-unit midpoint to a relative one
#'
#' @description brouillet expresses `midpoints`, `center` and `width` as
#'   fractions of the gradient range, while host functions usually hold a
#'   midpoint in data units (a Jaccard value of 1/3, an NMI value of 0). This
#'   converts the latter to the former, which is the single most common mistake
#'   when wiring brouillet into an existing plotting function.
#'
#' @param value Numeric midpoint(s) in data units, increasing.
#' @param limits Length-2 numeric giving the data range.
#' @param eps Values landing exactly on a limit are nudged inside by this much,
#'   since a stop at 0 or 1 is degenerate.
#' @return Numeric vector of relative positions, strictly inside (0, 1).
#' @export
#' @examples
#' relativeMidpoint(1 / 3, limits = c(0.2, 0.8))   # 0.2222
#' relativeMidpoint(0, limits = c(-0.1, 0.1))      # 0.5
relativeMidpoint <- function(value, limits, eps = 1e-6) {

  .check_limits(limits)

  if (!is.numeric(value) || !length(value) || anyNA(value)) {
    stop("'value' must be a non-missing numeric vector.", call. = FALSE)
  }
  if (length(value) > 1 && any(diff(value) <= 0)) {
    stop("'value' must be strictly increasing.", call. = FALSE)
  }

  rel <- (value - limits[1]) / diff(limits)

  if (any(rel < 0 | rel > 1)) {
    stop("Midpoint(s) fall outside 'limits': ", paste(format(value), collapse = ", "),
         " not within [", format(limits[1]), ", ", format(limits[2]), "].",
         call. = FALSE)
  }

  clamped <- pmin(1 - eps, pmax(eps, rel))

  if (!isTRUE(all.equal(clamped, rel))) {
    warning("Midpoint(s) sit on the edge of 'limits' and were nudged inside; ",
            "the resulting gradient has a near-degenerate segment.", call. = FALSE)
  }

  clamped
}
