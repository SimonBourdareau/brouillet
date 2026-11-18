#' brouillet: colour gradients for ggplot2
#'
#' @description
#' `brouillet` builds gradient *specifications* ([gradientParams()]) that are
#' independent of any particular plot, then materialises them either as a
#' data frame of value/colour pairs ([sigmoidGradient2()] and friends) or as a
#' ready-to-use `ggplot2` scale ([asScaleFillGradient()],
#' [asScaleColorGradient()]).
#'
#' Three gradient modes are available:
#' * `"sigmoid"` - the colour weight follows the product of two logistic
#'   curves, which concentrates the colour change in a chosen band of the
#'   data range;
#' * `"linear"` - piecewise-linear interpolation between stops;
#' * `"custom"` - any monotonic function mapping \[0, 1\] to \[0, 1\].
#'
#' @section Conventions:
#' Every gradient builder returns a data frame with a numeric `value` column
#' (in data units) and a character `color` column (hex strings), sorted by
#' `value`. `midpoints` are always *relative* positions in (0, 1) within
#' `low.value`--`high.value`, never data units.
#'
#' @keywords internal
#' @importFrom grDevices col2rgb colorRampPalette hsv rgb rgb2hsv
#' @importFrom ggplot2 aes element_blank geom_tile ggplot guide_colourbar
#' @importFrom ggplot2 scale_color_gradientn scale_fill_gradientn
#' @importFrom rlang .data
#' @importFrom stats approx
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous theme theme_minimal
"_PACKAGE"
