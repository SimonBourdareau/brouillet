## Internal helpers shared across the package. Nothing here is exported.

#' @noRd
.check_color <- function(color, arg) {
  # Length first: isColor() is vectorised, and `if` rejects length > 1.
  if (length(color) != 1 || !isColor(color)) {
    stop("'", arg, "' is not a single valid colour.", call. = FALSE)
  }
  invisible(TRUE)
}

#' @noRd
.check_colors <- function(colors, min_n = 2, arg = "colors") {
  if (is.null(colors) || !is.vector(colors) || length(colors) < min_n) {
    stop("'", arg, "' must contain at least ", min_n, " colours.", call. = FALSE)
  }
  ok <- isColor(colors)
  if (!all(ok)) {
    stop("Invalid colours at position(s): ",
         paste(which(!ok), collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

#' @noRd
.check_positive <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x <= 0) {
    stop("'", arg, "' must be a single positive number.", call. = FALSE)
  }
  invisible(TRUE)
}

#' @noRd
.check_unit_scalar <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 0 || x > 1) {
    stop("'", arg, "' must be a single number between 0 and 1.", call. = FALSE)
  }
  invisible(TRUE)
}

#' @noRd
.check_range <- function(low.value, high.value) {
  if (!is.numeric(low.value) || length(low.value) != 1 ||
      !is.numeric(high.value) || length(high.value) != 1 ||
      !is.finite(low.value) || !is.finite(high.value) ||
      low.value >= high.value) {
    stop("'low.value' must be a finite number less than 'high.value'.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Coerce resolution to a usable integer, warning if it was out of range
#'
#' @noRd
.fix_resolution <- function(resolution, min_res = 2, default = 100) {
  if (!is.numeric(resolution) || length(resolution) != 1 ||
      is.na(resolution) || resolution < min_res) {
    warning("'resolution' must be >= ", min_res, ". Using default value of ",
            default, ".", call. = FALSE)
    return(as.integer(default))
  }
  as.integer(floor(resolution))
}

#' Validate relative midpoints
#'
#' Midpoints are fractions of the gradient range, strictly inside (0, 1) and
#' strictly increasing.
#'
#' @noRd
.check_midpoints <- function(midpoints, n_colors) {
  expected <- n_colors - 2
  if (!is.numeric(midpoints) || length(midpoints) != expected) {
    stop("'midpoints' must be numeric of length length(colors) - 2 = ",
         expected, ".", call. = FALSE)
  }
  .check_midpoint_values(midpoints)
}

#' Validate midpoint values without reference to a palette length
#'
#' @noRd
.check_midpoint_values <- function(midpoints) {
  if (!is.numeric(midpoints)) {
    stop("'midpoints' must be numeric.", call. = FALSE)
  }
  if (any(!is.finite(midpoints)) || any(midpoints <= 0 | midpoints >= 1)) {
    stop("All midpoints must be strictly between 0 and 1 ",
         "(they are relative positions, not data units).", call. = FALSE)
  }
  if (length(midpoints) > 1 && any(diff(midpoints) <= 0)) {
    stop("Midpoints must be in strictly increasing order.", call. = FALSE)
  }
  invisible(TRUE)
}

#' @noRd
.check_gradient_df <- function(gradient) {
  if (!is.data.frame(gradient)) {
    stop("Argument 'gradient' must be a data frame.", call. = FALSE)
  }
  if (!all(c("value", "color") %in% names(gradient))) {
    stop("Argument 'gradient' must contain 'value' and 'color' columns.", call. = FALSE)
  }
  if (!is.numeric(gradient$value)) {
    stop("'value' column must be numeric.", call. = FALSE)
  }
  if (!is.character(gradient$color)) {
    stop("'color' column must be character.", call. = FALSE)
  }
  invisible(TRUE)
}

#' A small offset relative to the spread of a numeric vector
#'
#' Used to insert clip stops just outside a gradient without creating
#' duplicated breakpoints (which upset `scales::rescale()` interpolation).
#'
#' @noRd
.eps_for <- function(x, frac = 1e-3) {
  r <- diff(range(x))
  if (!is.finite(r) || r <= 0) return(frac)
  r * frac
}


#' Warn once per session per message id
#'
#' Deprecation warnings fire from inside gradient builders, which are often
#' called in loops; warning on every call would be unusable.
#'
#' @noRd
.brouillet_warned <- new.env(parent = emptyenv())

#' @noRd
.warn_once <- function(id, msg) {
  if (exists(id, envir = .brouillet_warned, inherits = FALSE)) {
    return(invisible(FALSE))
  }
  assign(id, TRUE, envir = .brouillet_warned)
  warning(msg, call. = FALSE)
  invisible(TRUE)
}
