########################################################################
## brouillet compatibility layer
##
## Source this file to run code written against the original (pre-0.2)
## snake_case API on top of brouillet >= 0.3. It is deliberately NOT part of
## the package: source it only in the scripts that need it.
##
##   library(brouillet)
##   source("brouillet-compat.R")
##
## Colours are interpolated in sRGB here, as the original did, so figures
## regenerate as they looked before. Everything reached through the modern API
## still uses Lab.
##
## Not reproduced: the original sigmoid_gradientn() built every segment
## backwards. Set fix.segments = TRUE for the corrected order.
########################################################################

.compat_presets <- list(
  very_low = c(500, 500), lower = c(350, 350), low = c(200, 200),
  low_longtail = c(500, 200), medium = c(100, 100), medium_longtail = c(500, 200),
  high = c(50, 50), very_high = c(25, 25), extreme = c(5, 5),
  extreme_longtail = c(500, 5)
)

.compat_sigmas <- function(sigma1, sigma2, steepness_preset) {
  if (!is.null(steepness_preset)) {
    if (!steepness_preset %in% names(.compat_presets)) {
      stop("Invalid steepness preset. Valid presets are: ",
           paste(names(.compat_presets), collapse = ", "), call. = FALSE)
    }
    p <- .compat_presets[[steepness_preset]]
    return(list(sigma1 = p[1], sigma2 = p[2]))
  }
  if (is.null(sigma1) || is.null(sigma2)) {
    stop("Either 'steepness_preset' or both 'sigma1' and 'sigma2' must be provided.",
         call. = FALSE)
  }
  list(sigma1 = sigma1, sigma2 = sigma2)
}

.compat_curve <- function(sigma1, sigma2, inflexpoint1, inflexpoint2) {
  m1 <- inflexpoint1 * 1000
  m2 <- inflexpoint2 * 1000
  function(x) {
    z <- pmin(1, pmax(0, x)) * 1000
    (1 / (1 + exp(-(z - m1) / sigma1))) * (1 / (1 + exp(-(z - m2) / sigma2)))
  }
}

.compat_rel <- function(values, low.value, high.value) {
  (values - low.value) / (high.value - low.value)
}

is_color <- function(color) brouillet::isColor(color)

colorUpdate <- function(color, color2 = NULL, shift = 50, resolution = 100) {
  brouillet::colorUpdate(color, color2, shift, resolution, space = "sRGB")
}

colorWheel <- function(color, num) brouillet::colorWheel(color, num)

colorComplementary <- function(color) brouillet::colorComplementary(color)

colorSpread <- function(color, spread = 3, resolution = 100) {
  brouillet::colorSpread(color, spread, resolution)
}

colorBlend <- function(color1, color2, weight = 0.5) {
  brouillet::colorBlend(color1, color2, weight, space = "sRGB")
}

custom_sigmoid <- function(x, sigma1, sigma2, midpoint1, midpoint2) {
  y1 <- 1 / (1 + exp(-(x - midpoint1) / sigma1))
  y2 <- 1 / (1 + exp(-(x - midpoint2) / sigma2))
  y1 * y2
}

sigmoid_gradient2 <- function(color1, color2, sigma1 = NULL, sigma2 = NULL,
                              inflexpoint1 = 0.3, inflexpoint2 = 0.4,
                              resolution = 100, steepness_preset = NULL,
                              low.value = 0, high.value = 1) {

  s <- .compat_sigmas(sigma1, sigma2, steepness_preset)
  df <- brouillet::customGradientFun(
    colors = c(color1, color2),
    mapping_fun = .compat_curve(s$sigma1, s$sigma2, inflexpoint1, inflexpoint2),
    resolution = floor(resolution),
    low.value = low.value, high.value = high.value,
    space = "sRGB"
  )
  attr(df, "gradient_type") <- "sigmoid_gradient2"
  df
}

sigmoid_gradient3 <- function(color1, color2, midpoint.color,
                              sigma1 = NULL, sigma2 = NULL,
                              inflexpoint1 = 0.3, inflexpoint2 = 0.4,
                              resolution = 100, steepness_preset = NULL,
                              low.value = 0, midpoint.value = 0.5, high.value = 1) {

  s <- .compat_sigmas(sigma1, sigma2, steepness_preset)
  curve <- .compat_curve(s$sigma1, s$sigma2, inflexpoint1, inflexpoint2)
  resolution <- floor(resolution)

  pos <- .compat_rel(midpoint.value, low.value, high.value)
  left_res <- max(2, round(resolution * pos))
  right_res <- max(2, resolution - left_res)

  left <- brouillet::customGradientFun(
    colors = c(midpoint.color, color1), mapping_fun = curve,
    resolution = left_res, low.value = low.value, high.value = midpoint.value,
    space = "sRGB")

  right <- brouillet::customGradientFun(
    colors = c(midpoint.color, color2), mapping_fun = curve,
    resolution = right_res, low.value = midpoint.value, high.value = high.value,
    space = "sRGB")

  df <- data.frame(
    value = c(seq(low.value, midpoint.value, length.out = left_res + 1)[-(left_res + 1)],
              seq(midpoint.value, high.value, length.out = right_res + 1)),
    color = c(rev(left$color[-(left_res + 1)]), right$color),
    stringsAsFactors = FALSE
  )
  attr(df, "gradient_type") <- "sigmoid_gradient3"
  df
}

sigmoid_gradientn <- function(colors, midpoints, sigma1 = NULL, sigma2 = NULL,
                              inflexpoint1 = 0.3, inflexpoint2 = 0.4,
                              resolution = 100, steepness_preset = NULL,
                              low.value = 0, high.value = 1,
                              fix.segments = FALSE) {

  s <- .compat_sigmas(sigma1, sigma2, steepness_preset)
  curve <- .compat_curve(s$sigma1, s$sigma2, inflexpoint1, inflexpoint2)
  resolution <- floor(resolution)

  stops <- c(low.value, midpoints, high.value)
  segment_resolutions <- pmax(2, round(resolution * base::diff(stops) /
                                         (high.value - low.value)))

  parts <- vector("list", length(colors) - 1)
  for (i in seq_along(parts)) {
    pair <- if (fix.segments) c(colors[i], colors[i + 1]) else c(colors[i + 1], colors[i])
    parts[[i]] <- brouillet::customGradientFun(
      colors = pair, mapping_fun = curve,
      resolution = segment_resolutions[i],
      low.value = stops[i], high.value = stops[i + 1],
      space = "sRGB")
  }

  df <- do.call(rbind, lapply(parts, function(seg) seg[-nrow(seg), ]))
  df <- rbind(df, parts[[length(parts)]][nrow(parts[[length(parts)]]), ])
  rownames(df) <- NULL
  attr(df, "gradient_type") <- "sigmoid_gradientn"
  df
}

linear_gradient2 <- function(color1, color2, inflexpoint1 = 0, inflexpoint2 = 1,
                             resolution = 100, low.value = 0, high.value = 1) {

  df <- brouillet::linearGradient2(
    color1, color2,
    center = (inflexpoint1 + inflexpoint2) / 2,
    width = inflexpoint2 - inflexpoint1,
    resolution = floor(resolution),
    low.value = low.value, high.value = high.value,
    space = "sRGB", rescale = FALSE
  )
  attr(df, "gradient_type") <- "linear_gradient2"
  df
}

linear_gradientn <- function(colors, midpoints, resolution = 100,
                             low.value = 0, high.value = 1) {

  rel <- if (length(colors) > 2) .compat_rel(midpoints, low.value, high.value) else NULL
  df <- brouillet::linearGradientn(
    colors, midpoints = rel, resolution = floor(resolution),
    low.value = low.value, high.value = high.value,
    space = "sRGB", rescale = FALSE
  )
  attr(df, "gradient_type") <- "linear_gradientn"
  df
}

custom_gradient_fun <- function(colors, mapping_fun, resolution = 100,
                                low.value = 0, high.value = 1) {
  brouillet::customGradientFun(colors, mapping_fun, floor(resolution),
                               low.value, high.value, space = "sRGB")
}

clipping_gradient <- function(gradient, low.value, high.value,
                              low.clip.color, high.clip.color, resolution = 100) {
  brouillet::clippingGradient(gradient, low.value, high.value,
                              low.clip.color, high.clip.color)
}

blending_clipping_gradient <- function(gradient, low.value, high.value,
                                       low.clip.color, high.clip.color,
                                       resolution = 100) {
  brouillet::blendingClippingGradient(gradient, low.value, high.value,
                                      low.clip.color, high.clip.color, resolution)
}

gradient_params <- function(colors = NULL, midpoints = NULL,
                            sigma1 = NULL, sigma2 = NULL,
                            inflexpoint1 = 0.3, inflexpoint2 = 0.4,
                            resolution = 100, steepness_preset = "low",
                            low.clip.color = NULL, high.clip.color = NULL,
                            blend.clip = FALSE, gradient_mode = "sigmoid",
                            mapping_fun = NULL) {

  if (is.null(colors) || length(colors) < 2) {
    stop("'colors' must contain at least 2 colors.", call. = FALSE)
  }
  ok <- brouillet::isColor(colors)
  if (!all(ok)) {
    stop("Invalid colors at position(s): ", paste(which(!ok), collapse = ", "),
         call. = FALSE)
  }

  structure(list(colors = colors, midpoints = midpoints,
                 center = NA_real_, width = NA_real_, asymmetry = 0,
                 space = "sRGB", interpolate = "linear", rescale = FALSE,
                 sigma1 = sigma1, sigma2 = sigma2,
                 inflexpoint1 = inflexpoint1, inflexpoint2 = inflexpoint2,
                 resolution = floor(resolution), steepness_preset = steepness_preset,
                 low.clip.color = low.clip.color, high.clip.color = high.clip.color,
                 blend.clip = blend.clip, gradient_mode = gradient_mode,
                 mapping_fun = mapping_fun, created = Sys.time()),
            class = c("brouillet_legacy_params", "brouillet_gradient_params"))
}

print.brouillet_legacy_params <- function(x, ...) {
  cat("<brouillet gradient (legacy compatibility object)>\n")
  cat(" mode:       ", x$gradient_mode, "\n", sep = "")
  cat(" colors:     ", paste(x$colors, collapse = " -> "), "\n", sep = "")
  cat(" resolution: ", x$resolution, "\n", sep = "")
  if (!is.null(x$midpoints)) {
    cat(" midpoints:  ", paste(x$midpoints, collapse = ", "), " (data units)\n", sep = "")
  }
  if (!is.null(x$low.clip.color) || !is.null(x$high.clip.color)) {
    cat(" clipping:   ", x$low.clip.color, " | ", x$high.clip.color, "\n", sep = "")
  }
  invisible(x)
}

.build_gradient <- function(params, low.value, high.value) {

  if (!inherits(params, "brouillet_legacy_params")) {
    return(brouillet::buildGradient(params, c(low.value, high.value)))
  }

  n <- length(params$colors)
  mode <- params$gradient_mode

  args <- list(sigma1 = params$sigma1, sigma2 = params$sigma2,
               inflexpoint1 = params$inflexpoint1, inflexpoint2 = params$inflexpoint2,
               resolution = params$resolution,
               steepness_preset = params$steepness_preset,
               low.value = low.value, high.value = high.value)

  if (mode == "sigmoid") {
    if (n == 2) {
      df <- do.call(sigmoid_gradient2,
                    c(list(color1 = params$colors[1], color2 = params$colors[2]), args))
    } else if (n == 3) {
      mid <- if (is.null(params$midpoints)) mean(c(low.value, high.value)) else params$midpoints
      df <- do.call(sigmoid_gradient3,
                    c(list(color1 = params$colors[1], midpoint.color = params$colors[2],
                           color2 = params$colors[3], midpoint.value = mid), args))
    } else {
      df <- do.call(sigmoid_gradientn,
                    c(list(colors = params$colors, midpoints = params$midpoints), args))
    }
  } else if (mode == "linear") {
    if (n == 2) {
      df <- linear_gradient2(params$colors[1], params$colors[2],
                             params$inflexpoint1, params$inflexpoint2,
                             params$resolution, low.value, high.value)
    } else {
      df <- linear_gradientn(params$colors, params$midpoints,
                             params$resolution, low.value, high.value)
    }
  } else {
    df <- custom_gradient_fun(params$colors, params$mapping_fun,
                              params$resolution, low.value, high.value)
  }

  if (!is.null(params$low.clip.color) || !is.null(params$high.clip.color)) {
    lo <- if (is.null(params$low.clip.color)) df$color[1] else params$low.clip.color
    hi <- if (is.null(params$high.clip.color)) df$color[nrow(df)] else params$high.clip.color
    df <- if (isTRUE(params$blend.clip)) {
      brouillet::blendingClippingGradient(df, low.value, high.value, lo, hi,
                                          resolution = params$resolution)
    } else {
      brouillet::clippingGradient(df, low.value, high.value, lo, hi)
    }
  }

  df
}

reverse_gradient <- function(params) {
  if (!inherits(params, "brouillet_legacy_params")) {
    return(brouillet::reverseGradient(params))
  }
  new_params <- params
  new_params$colors <- rev(params$colors)
  if (!is.null(params$midpoints)) new_params$midpoints <- 1 - rev(params$midpoints)
  new_params$low.clip.color <- params$high.clip.color
  new_params$high.clip.color <- params$low.clip.color
  new_params
}

.compat_scale_pieces <- function(params, limits, show.clip, clip.width) {

  df <- .build_gradient(params, limits[1], limits[2])
  values <- df$value
  colours <- df$color

  has_clip <- !is.null(params$low.clip.color) || !is.null(params$high.clip.color)

  if (isTRUE(show.clip) && has_clip) {
    w <- base::diff(limits) * clip.width
    eps <- max(w * 1e-3, .Machine$double.eps)
    lo <- if (is.null(params$low.clip.color)) colours[1] else params$low.clip.color
    hi <- if (is.null(params$high.clip.color)) colours[length(colours)] else params$high.clip.color
    values <- c(limits[1] - w, min(values) - eps, values, max(values) + eps, limits[2] + w)
    colours <- c(lo, lo, colours, hi, hi)
  }

  ord <- order(values)
  values <- values[ord]
  colours <- colours[ord]
  keep <- !duplicated(values)

  list(values = values[keep], colours = colours[keep],
       limits = range(values[keep]))
}

as_scale_fill_gradient <- function(params, limits = c(0, 1), na.value = "grey95",
                                   guide = ggplot2::guide_colourbar(),
                                   show.clip = TRUE, clip.width = 0.02, ...) {

  if (!inherits(params, "brouillet_legacy_params")) {
    return(brouillet::asScaleFillGradient(params, limits, na.value, guide,
                                          show.clip, clip.width, ...))
  }
  pieces <- .compat_scale_pieces(params, limits, show.clip, clip.width)
  dots <- list(...)
  if (is.null(dots$oob)) dots$oob <- scales::oob_squish
  do.call(ggplot2::scale_fill_gradientn,
          c(list(colours = pieces$colours, values = scales::rescale(pieces$values),
                 limits = pieces$limits, na.value = na.value, guide = guide), dots))
}

as_scale_color_gradient <- function(params, limits = c(0, 1), na.value = "grey95",
                                    guide = ggplot2::guide_colourbar(),
                                    show.clip = TRUE, clip.width = 0.02, ...) {

  if (!inherits(params, "brouillet_legacy_params")) {
    return(brouillet::asScaleColorGradient(params, limits, na.value, guide,
                                           show.clip, clip.width, ...))
  }
  pieces <- .compat_scale_pieces(params, limits, show.clip, clip.width)
  dots <- list(...)
  if (is.null(dots$oob)) dots$oob <- scales::oob_squish
  do.call(ggplot2::scale_color_gradientn,
          c(list(colours = pieces$colours, values = scales::rescale(pieces$values),
                 limits = pieces$limits, na.value = na.value, guide = guide), dots))
}

preview_gradient <- function(params, limits = c(0, 1), n_ticks = 5) {
  if (!inherits(params, "brouillet_legacy_params")) {
    return(brouillet::previewGradient(params, limits, n_ticks))
  }
  df <- data.frame(value = seq(limits[1], limits[2], length.out = 500), y = 1)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$value, y = .data$y, fill = .data$value)) +
    ggplot2::geom_tile(height = 1) +
    as_scale_fill_gradient(params, limits = limits) +
    ggplot2::scale_y_continuous(NULL, breaks = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(axis.title = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   legend.position = "none")
}
