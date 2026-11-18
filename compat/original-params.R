clipping_gradient <- function(gradient, low.value, high.value, low.clip.color, high.clip.color, resolution = 100) {
  rbind(
    data.frame(value = min(gradient$value) - 0.001, color = low.clip.color),
    gradient,
    data.frame(value = max(gradient$value) + 0.001, color = high.clip.color)
  )
}

blending_clipping_gradient <- function(gradient, low.value, high.value,
                                       low.clip.color, high.clip.color,
                                       resolution = 100) {
  n_blend <- max(5, floor(resolution * 0.1))
  low_first <- gradient$color[1]
  high_last <- gradient$color[nrow(gradient)]
  low_blend <- colorRampPalette(c(low.clip.color, low_first))(n_blend + 2)[1:n_blend]
  high_blend <- colorRampPalette(c(high_last, high.clip.color))(n_blend + 2)[1:n_blend]
  step <- diff(range(gradient$value)) / nrow(gradient)
  low_vals <- seq(min(gradient$value) - n_blend * step, min(gradient$value) - step, length.out = n_blend)
  high_vals <- seq(max(gradient$value) + step, max(gradient$value) + n_blend * step, length.out = n_blend)
  rbind(
    data.frame(value = low_vals, color = low_blend),
    gradient,
    data.frame(value = high_vals, color = high_blend)
  )
}

gradient_params <- function(colors = NULL, midpoints = NULL, sigma1 = NULL, sigma2 = NULL,
                            inflexpoint1 = 0.3, inflexpoint2 = 0.4, resolution = 100,
                            steepness_preset = "low", low.clip.color = NULL,
                            high.clip.color = NULL, blend.clip = FALSE,
                            gradient_mode = "sigmoid", mapping_fun = NULL) {
  structure(list(colors = colors, midpoints = midpoints, sigma1 = sigma1, sigma2 = sigma2,
                 inflexpoint1 = inflexpoint1, inflexpoint2 = inflexpoint2,
                 resolution = resolution, steepness_preset = steepness_preset,
                 low.clip.color = low.clip.color, high.clip.color = high.clip.color,
                 blend.clip = blend.clip, gradient_mode = gradient_mode,
                 mapping_fun = mapping_fun, created = Sys.time()),
            class = "brouillet_gradient_params")
}

.build_gradient <- function(params, low.value, high.value) {

  n <- length(params$colors)
  mode <- params$gradient_mode

  base_args <- list(
    sigma1 = params$sigma1,
    sigma2 = params$sigma2,
    inflexpoint1 = params$inflexpoint1,
    inflexpoint2 = params$inflexpoint2,
    resolution = params$resolution,
    steepness_preset = params$steepness_preset,
    low.value = low.value,
    high.value = high.value
  )

  if (mode == "sigmoid") {
    if (n == 2) {
      gradient_df <- do.call(sigmoid_gradient2,
                             c(list(color1 = params$colors[1], color2 = params$colors[2]), base_args))
    } else if (n == 3) {
      midpoint.value <- if (is.null(params$midpoints)) mean(c(low.value, high.value)) else params$midpoints
      gradient_df <- do.call(sigmoid_gradient3,
                             c(list(color1 = params$colors[1], midpoint.color = params$colors[2],
                                    color2 = params$colors[3], midpoint.value = midpoint.value), base_args))
    } else {
      gradient_df <- do.call(sigmoid_gradientn,
                             c(list(colors = params$colors, midpoints = params$midpoints), base_args))
    }
  } else if (mode == "linear") {
    if (n == 2) {
      gradient_df <- linear_gradient2(params$colors[1], params$colors[2],
                                      params$inflexpoint1, params$inflexpoint2,
                                      params$resolution, low.value, high.value)
    } else {
      gradient_df <- linear_gradientn(params$colors, params$midpoints,
                                      params$resolution, low.value, high.value)
    }
  } else if (mode == "custom") {
    gradient_df <- custom_gradient_fun(params$colors, params$mapping_fun,
                                       params$resolution, low.value, high.value)
  }

  if (!is.null(params$low.clip.color) || !is.null(params$high.clip.color)) {
    if (isTRUE(params$blend.clip)) {
      gradient_df <- blending_clipping_gradient(gradient_df, low.value, high.value,
                                                params$low.clip.color, params$high.clip.color,
                                                resolution = params$resolution)
    } else {
      gradient_df <- clipping_gradient(gradient_df, low.value, high.value,
                                       params$low.clip.color, params$high.clip.color)
    }
  }

  gradient_df
}
