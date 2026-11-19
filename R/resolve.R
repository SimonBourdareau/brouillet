#' Does a specification define out-of-range colours?
#'
#' @param params A `brouillet_gradient_params` object.
#' @return `TRUE` when either clip colour is set.
#' @details Mainly useful in wrappers that squish data before plotting: squished
#'   values are never out of range, so the clip colours can never appear.
#' @export
#' @examples
#' hasClipColors(gradientParams(c("navy", "firebrick")))
#' hasClipColors(gradientParams(c("navy", "firebrick"), high.clip.color = "gold"))
hasClipColors <- function(params) {
  if (!inherits(params, c("brouillet_gradient_params", "brouillet_gradient_template"))) {
    stop("'params' must be a gradient specification or template.", call. = FALSE)
  }
  !is.null(params$low.clip.color) || !is.null(params$high.clip.color)
}

#' Resolve whatever a caller supplied into a specification
#'
#' @description Turns the gradient argument of a plotting function into a
#'   validated specification, whichever form the caller used: nothing, a
#'   template, or a finished specification. Written for wrappers that expose
#'   their own colour arguments and a midpoint in data units.
#'
#' @param spec `NULL`, a `brouillet_gradient_template`, or a
#'   `brouillet_gradient_params` object (including one from
#'   [gradient_params()]).
#' @param colors Fallback palette, used when `spec` is `NULL` or carries no
#'   colours. Two or three colours; a `NULL` middle colour collapses the vector,
#'   so `c(low, mid, high)` handles both cases.
#' @param limits Numeric vector of length 2: the data range the gradient will be
#'   drawn over. Required when `midpoint` is given.
#' @param midpoint Position of the middle colour **in data units**, or `NULL`.
#'   Ignored unless `colors` has three entries.
#' @param gradient.mode Mode for the specification built when `spec` is `NULL`.
#' @param quiet Suppress the message explaining why a midpoint was dropped.
#' @return A `brouillet_gradient_params` object ready for [asColorFun()],
#'   [asScaleFillGradient()] or [buildGradient()].
#' @details
#' Three things make this fiddly enough to be worth a function:
#'
#' * `midpoints` are fractions of the range, while a plotting function's
#'   `midpoint` argument is usually in data units. The conversion needs
#'   `limits`, which is why they are an argument here.
#' * A midpoint can land on or outside a limit, which no interior stop can
#'   represent. The middle colour is dropped and a message explains why.
#' * Specifications from [gradient_params()] read `midpoints` as data units, so
#'   they need the opposite treatment. That is handled here.
#'
#' When `spec` already carries colours they win; only its interior stop is
#' updated, and only for a three-colour palette. Set one clip colour and the
#' other is filled from the matching end of the ramp, so the colourbar does not
#' gain a band on one side only.
#' @export
#' @examples
#' resolveGradient(NULL, c("white", "pink", "red"), limits = c(0, 1), midpoint = 0.5)
#'
#' resolveGradient(gradientTemplate(width = 0.2), c("white", "pink", "red"),
#'                 limits = c(0, 100), midpoint = 50)
#'
#' # A midpoint on a limit cannot be an interior stop
#' resolveGradient(NULL, c("white", "pink", "red"), limits = c(0, 1), midpoint = 0)
#'
#' # A caller's own specification is respected
#' resolveGradient(gradientParams(c("navy", "gold")), c("white", "pink", "red"),
#'                 limits = c(0, 1), midpoint = 0.5)$colors
resolveGradient <- function(spec = NULL, colors, limits = NULL, midpoint = NULL,
                            gradient.mode = "linear", quiet = FALSE) {

  .check_colors(colors, min_n = 2)

  if (!is.null(midpoint)) {
    if (is.null(limits)) {
      stop("'limits' is required when 'midpoint' is given.", call. = FALSE)
    }
    .check_limits(limits)
  }

  use_mid <- length(colors) >= 3 && !is.null(midpoint) && length(midpoint) == 1 &&
    is.finite(midpoint) && midpoint > limits[1] && midpoint < limits[2]

  if (length(colors) >= 3 && !use_mid) {
    if (!quiet) {
      message("'midpoint' is missing or sits on a scale limit; ",
              "using the first and last colours only.")
    }
    colors <- colors[c(1, length(colors))]
  }

  legacy <- isTRUE(spec$legacy)

  stop_position <- if (!use_mid) {
    NULL
  } else if (legacy) {
    midpoint
  } else {
    relativeMidpoint(midpoint, limits)
  }

  if (is.null(spec)) {
    return(gradientParams(colors, midpoints = stop_position,
                          gradient.mode = gradient.mode))
  }

  if (!inherits(spec, c("brouillet_gradient_params", "brouillet_gradient_template"))) {
    stop("'spec' must come from gradientParams(), gradientTemplate() or ",
         "gradient_params().", call. = FALSE)
  }

  if (is.null(spec$colors)) {
    spec <- withColors(spec, colors, midpoints = stop_position)
  } else if (length(spec$colors) == 3 && use_mid) {
    spec <- updateGradient(spec, midpoints = stop_position)
  }

  if (!is.null(spec$low.clip.color) && is.null(spec$high.clip.color)) {
    spec <- updateGradient(spec, high.clip.color = spec$colors[length(spec$colors)])
  }
  if (is.null(spec$low.clip.color) && !is.null(spec$high.clip.color)) {
    spec <- updateGradient(spec, low.clip.color = spec$colors[1])
  }

  spec
}
