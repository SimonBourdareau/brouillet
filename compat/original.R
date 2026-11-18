is_color <- function(color) {
  tryCatch({ col2rgb(color); TRUE }, error = function(e) FALSE)
}

colorUpdate <- function(color, color2 = NULL, shift = 50, resolution = 100) {
  if (!is_color(color)) stop("The 'color' argument is not a valid color.")
  if (!is.numeric(shift) || length(shift) != 1) stop("'shift' must be a single numeric value.")
  if (!is.numeric(resolution) || resolution < 2) { warning("res"); resolution <- 100 }
  resolution <- floor(resolution)
  shift <- max(0, min(resolution, shift))
  base_color2 <- "white"
  if (is.null(color2)) {
    colorVector <- c(base_color2, color)
  } else if (is.character(color2) && length(color2) == 1) {
    if (color2 == "complementary") {
      colorVector <- c(colorComplementary(color), color)
    } else if (is_color(color2)) {
      colorVector <- c(color2, color)
    } else stop("'color2' must be a valid color or 'complementary'.")
  } else if (is.numeric(color2) && length(color2) == 1 && color2 >= 0) {
    colorVector <- c(color, colors_spread(color, spread = color2))
  } else stop("'color2' must be character, numeric, or NULL.")
  f <- colorRampPalette(colorVector)
  index <- floor((shift / resolution) * (resolution - 1)) + 1
  f(resolution)[index]
}

colorWheel <- function(color, num) {
  if (!is_color(color)) stop("bad color")
  num <- as.integer(num)
  hsv_col <- rgb2hsv(col2rgb(color))[, 1]
  cols <- seq(hsv_col[1], hsv_col[1] + 1 - 1 / num, length.out = num)
  cols[cols > 1] <- cols[cols > 1] - 1
  colors <- hsv(cols, hsv_col[2], hsv_col[3])
  if (substr(color, 1, 1) == "#" && nchar(color) == 9) {
    colors <- paste0(colors, substr(color, 8, 9))
  }
  colors
}

colorComplementary <- function(color) {
  if (!is_color(color)) stop("bad color")
  hsv_col <- rgb2hsv(col2rgb(color))[, 1]
  comp_hue <- hsv_col[1] + 0.5
  if (comp_hue > 1) comp_hue <- comp_hue - 1
  comp_color <- hsv(comp_hue, hsv_col[2], hsv_col[3])
  if (substr(color, 1, 1) == "#" && nchar(color) == 9) {
    comp_color <- paste0(comp_color, substr(color, 8, 9))
  }
  comp_color
}

colorSpread <- function(color, spread = 3, resolution = 100) {
  spread <- floor(spread)
  if (!is.numeric(resolution) || resolution < spread) resolution <- spread * 10
  resolution <- floor(resolution)
  tmp_cols <- colorWheel(color, resolution)
  step <- floor(resolution / spread)
  tmp_cols[seq(step, resolution - step, by = step)]
}

colorBlend <- function(color1, color2, weight = 0.5) {
  rgb1 <- col2rgb(color1); rgb2 <- col2rgb(color2)
  blended <- rgb1 * weight + rgb2 * (1 - weight)
  rgb(blended[1], blended[2], blended[3], maxColorValue = 255)
}

custom_sigmoid <- function(x, sigma1, sigma2, midpoint1, midpoint2) {
  y1 <- 1 / (1 + exp(-(x - midpoint1) / sigma1))
  y2 <- 1 / (1 + exp(-(x - midpoint2) / sigma2))
  y1 * y2
}

PRESETS <- list(
  very_low = list(sigma1 = 500, sigma2 = 500),
  lower = list(sigma1 = 350, sigma2 = 350),
  low = list(sigma1 = 200, sigma2 = 200),
  low_longtail = list(sigma1 = 500, sigma2 = 200),
  medium = list(sigma1 = 100, sigma2 = 100),
  medium_longtail = list(sigma1 = 500, sigma2 = 200),
  high = list(sigma1 = 50, sigma2 = 50),
  very_high = list(sigma1 = 25, sigma2 = 25),
  extreme = list(sigma1 = 5, sigma2 = 5),
  extreme_longtail = list(sigma1 = 500, sigma2 = 5)
)

sigmoid_gradient2 <- function(color1, color2, sigma1 = NULL, sigma2 = NULL,
                              inflexpoint1 = 0.3, inflexpoint2 = 0.4,
                              resolution = 100, steepness_preset = NULL,
                              low.value = 0, high.value = 1) {
  resolution <- floor(resolution)
  if (!is.null(steepness_preset)) {
    sigma1 <- PRESETS[[steepness_preset]]$sigma1
    sigma2 <- PRESETS[[steepness_preset]]$sigma2
  }
  x_seq <- seq(0, 1000, length.out = resolution + 1)
  new_values <- custom_sigmoid(x_seq, sigma1, sigma2,
                               inflexpoint1 * 1000, inflexpoint2 * 1000)
  color_vec <- vapply(new_values, function(value) {
    colorUpdate(color = color2, color2 = color1, shift = value * 100, resolution = 100)
  }, character(1))
  data.frame(value = seq(low.value, high.value, length.out = resolution + 1),
             color = color_vec, stringsAsFactors = FALSE)
}

sigmoid_gradient3 <- function(color1, color2, midpoint.color, sigma1 = NULL, sigma2 = NULL,
                              inflexpoint1 = 0.3, inflexpoint2 = 0.4, resolution = 100,
                              steepness_preset = NULL, low.value = 0,
                              midpoint.value = 0.5, high.value = 1) {
  resolution <- floor(resolution)
  if (!is.null(steepness_preset)) {
    sigma1 <- PRESETS[[steepness_preset]]$sigma1
    sigma2 <- PRESETS[[steepness_preset]]$sigma2
  }
  total_range <- high.value - low.value
  midpoint_position <- (midpoint.value - low.value) / total_range
  left_res <- max(2, round(resolution * midpoint_position))
  right_res <- max(2, resolution - left_res)
  m1 <- inflexpoint1 * 1000; m2 <- inflexpoint2 * 1000
  left_values <- custom_sigmoid(seq(0, 1000, length.out = left_res + 1), sigma1, sigma2, m1, m2)
  right_values <- custom_sigmoid(seq(0, 1000, length.out = right_res + 1), sigma1, sigma2, m1, m2)
  color_vec_left <- vapply(left_values, function(value) {
    colorUpdate(color = color1, color2 = midpoint.color, shift = value * 100, resolution = 100)
  }, character(1))
  color_vec_right <- vapply(right_values, function(value) {
    colorUpdate(color = color2, color2 = midpoint.color, shift = value * 100, resolution = 100)
  }, character(1))
  left_range <- seq(low.value, midpoint.value, length.out = left_res + 1)
  right_range <- seq(midpoint.value, high.value, length.out = right_res + 1)
  data.frame(value = c(left_range[-length(left_range)], right_range),
             color = c(rev(color_vec_left[-length(color_vec_left)]), color_vec_right),
             stringsAsFactors = FALSE)
}

sigmoid_gradientn <- function(colors, midpoints, sigma1 = NULL, sigma2 = NULL,
                              inflexpoint1 = 0.3, inflexpoint2 = 0.4, resolution = 100,
                              steepness_preset = NULL, low.value = 0, high.value = 1) {
  resolution <- floor(resolution)
  segment_positions <- c(low.value, midpoints, high.value)
  total_range <- high.value - low.value
  segment_ranges <- base::diff(segment_positions)
  segment_resolutions <- pmax(2, round(resolution * segment_ranges / total_range))
  df_list <- vector("list", length(colors) - 1)
  for (i in seq_along(df_list)) {
    df_list[[i]] <- sigmoid_gradient2(
      color1 = colors[i + 1], color2 = colors[i],
      sigma1 = sigma1, sigma2 = sigma2,
      inflexpoint1 = inflexpoint1, inflexpoint2 = inflexpoint2,
      resolution = segment_resolutions[i], steepness_preset = steepness_preset,
      low.value = segment_positions[i], high.value = segment_positions[i + 1])
  }
  df <- do.call(rbind, lapply(df_list, function(seg) seg[-nrow(seg), ]))
  rbind(df, df_list[[length(df_list)]][nrow(df_list[[length(df_list)]]), ])
}

linear_gradient2 <- function(color1, color2, inflexpoint1 = 0, inflexpoint2 = 1,
                             resolution = 100, low.value = 0, high.value = 1) {
  resolution <- floor(resolution)
  x <- seq(0, 1, length.out = resolution + 1)
  w <- numeric(length(x))
  w[x <= inflexpoint1] <- 0
  w[x >= inflexpoint2] <- 1
  mid <- x > inflexpoint1 & x < inflexpoint2
  w[mid] <- (x[mid] - inflexpoint1) / (inflexpoint2 - inflexpoint1)
  cols <- vapply(w, function(a) colorBlend(color2, color1, weight = a), character(1))
  data.frame(value = seq(low.value, high.value, length.out = resolution + 1),
             color = cols, stringsAsFactors = FALSE)
}

linear_gradientn <- function(colors, midpoints, resolution = 100,
                             low.value = 0, high.value = 1) {
  resolution <- floor(resolution)
  n <- length(colors)
  stops <- if (n == 2) c(low.value, high.value) else c(low.value, midpoints, high.value)
  total_range <- high.value - low.value
  segment_resolutions <- pmax(2, round(resolution * base::diff(stops) / total_range))
  out <- vector("list", n - 1)
  for (i in seq_along(out)) {
    out[[i]] <- linear_gradient2(colors[i], colors[i + 1], 0, 1,
                                 segment_resolutions[i], stops[i], stops[i + 1])
  }
  df <- do.call(rbind, lapply(out, function(seg) seg[-nrow(seg), ]))
  rbind(df, out[[length(out)]][nrow(out[[length(out)]]), ])
}

custom_gradient_fun <- function(colors, mapping_fun, resolution = 100,
                                low.value = 0, high.value = 1) {
  resolution <- floor(resolution)
  x <- seq(0, 1, length.out = resolution + 1)
  w <- mapping_fun(x)
  all_colors <- colorRampPalette(colors)(resolution + 1)
  indices <- pmax(1, pmin(resolution + 1, floor(w * resolution) + 1))
  data.frame(value = seq(low.value, high.value, length.out = resolution + 1),
             color = all_colors[indices], stringsAsFactors = FALSE)
}
