# Generates man/figures/README-gradients.png and prints the perceptual
# uniformity table quoted in the README.
#
#   Rscript data-raw/make-readme-figure.R
#
# Deliberately compares brouillet against the alternatives rather than only
# showing it off: if a panel does not favour brouillet, that is information.

for (pkg in c("ggplot2", "patchwork", "colorspace", "farver")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("This script needs the suggested package '", pkg, "'.", call. = FALSE)
  }
}

library(ggplot2)
library(patchwork)
library(brouillet)

set.seed(1)
dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)

DIV <- c("navy", "white", "firebrick")
N <- 256          # for drawing the strips
N_DE <- 64        # for the uniformity table: at 256 samples the per-step
                  # distances are the same size as 8-bit rounding jitter, so
                  # the statistic measures quantisation rather than the ramp
t <- seq(0, 1, length.out = N)

# ---- the four ways of getting a diverging ramp -------------------------

methods <- list(
  `brouillet 0.1 (sRGB stops)` = {
    p <- gradientParams(DIV, midpoints = 0.5, space = "sRGB",
                         gradient_mode = "linear")
    asColorFun(p)(t)
  },
  `plain ggplot2 scale_*_gradientn()` = {
    # What ggplot2 does with stops you hand it: scales::colour_ramp(), Lab.
    scales::gradient_n_pal(DIV)(t)
  },
  `brouillet 0.2 (Lab stops)` = {
    p <- gradientParams(DIV, midpoints = 0.5, gradient_mode = "linear")
    asColorFun(p)(t)
  },
  `colorspace divergingx` = colorspace::divergingx_hcl(N, "RdBu", rev = TRUE),
  `brouillet + contrastCurve(width = 0.25)` = {
    p <- gradientParams(DIV, midpoints = 0.5, width = 0.25)
    asColorFun(p)(t)
  }
)

# ---- panel A: the curve ------------------------------------------------

curve_df <- do.call(rbind, lapply(
  list(c(0.5, 0.9, 0), c(0.5, 0.4, 0), c(0.5, 0.12, 0),
       c(0.5, 0.4, 0.8), c(0.3, 0.4, 0)),
  function(p) {
    data.frame(
      x = t,
      y = contrastCurve(t, center = p[1], width = p[2], asymmetry = p[3]),
      label = sprintf("center %.2g, width %.2g, asym %.2g", p[1], p[2], p[3])
    )
  }
))

pA <- ggplot(curve_df, aes(x, y, colour = label)) +
  geom_abline(slope = 1, intercept = 0, linetype = 3, colour = "grey60") +
  geom_line(linewidth = 0.7) +
  scale_colour_brewer(palette = "Dark2", name = NULL) +
  labs(x = "position in data range", y = "position in colour ramp",
       title = "A. contrastCurve(): where the colour change happens") +
  theme_minimal(base_size = 9) +
  theme(legend.position = "bottom", legend.key.width = unit(18, "pt"),
        panel.grid.minor = element_blank()) +
  guides(colour = guide_legend(nrow = 2))

# ---- panel B: the ramps themselves ------------------------------------

strip_df <- do.call(rbind, lapply(names(methods), function(nm) {
  data.frame(x = t, method = nm, fill = methods[[nm]])
}))
strip_df$method <- factor(strip_df$method, levels = rev(names(methods)))

pB <- ggplot(strip_df, aes(x, 1, fill = fill)) +
  geom_raster() +
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(NULL, breaks = NULL, expand = c(0, 0)) +
  facet_wrap(~method, ncol = 1, strip.position = "left") +
  labs(x = NULL, title = "B. The same three colours, five ways") +
  theme_minimal(base_size = 9) +
  theme(strip.text.y.left = element_text(angle = 0, hjust = 1, size = 7),
        panel.spacing = unit(2, "pt"), panel.grid = element_blank())

# ---- panel C: perceptual uniformity ------------------------------------

# Step-to-step distance in Lab. A flat line means equal data steps look like
# equal colour steps; spikes and dead zones are where readers see structure
# that is not in the data.
step_de <- function(cols) {
  lab <- farver::convert_colour(t(grDevices::col2rgb(cols)), "rgb", "lab")
  sqrt(rowSums(diff(lab)^2))
}

# Same five methods, resampled coarsely for the statistics.
t_de <- seq(0, 1, length.out = N_DE)
methods_de <- list(
  asColorFun(gradientParams(DIV, midpoints = 0.5, space = "sRGB",
                               gradient_mode = "linear"))(t_de),
  scales::gradient_n_pal(DIV)(t_de),
  asColorFun(gradientParams(DIV, midpoints = 0.5,
                               gradient_mode = "linear"))(t_de),
  colorspace::divergingx_hcl(N_DE, "RdBu", rev = TRUE),
  asColorFun(gradientParams(DIV, midpoints = 0.5, width = 0.25))(t_de)
)
names(methods_de) <- names(methods)

de <- lapply(methods, step_de)
de_coarse <- lapply(methods_de, step_de)
de_df <- do.call(rbind, lapply(names(de_coarse), function(nm) {
  data.frame(x = t_de[-1], de = de_coarse[[nm]], method = nm)
}))

uniformity <- data.frame(
  method = names(de_coarse),
  mean_dE = vapply(de_coarse, mean, numeric(1)),
  cv = vapply(de_coarse, function(d) sd(d) / mean(d), numeric(1)),
  # Ratio of the 95th to the 5th percentile: a robust spread measure. max/min
  # is useless here because a flat region gives steps of exactly zero.
  p95_p05 = vapply(de_coarse, function(d) {
    unname(quantile(d, 0.95) / max(quantile(d, 0.05), 1e-9))
  }, numeric(1)),
  flat_steps = vapply(de_coarse, function(d) sum(d < 1e-9), numeric(1)),
  row.names = NULL
)
uniformity <- uniformity[order(uniformity$cv), ]
print(uniformity, digits = 3)
utils::write.csv(uniformity, "data-raw/uniformity.csv", row.names = FALSE)

pC <- ggplot(de_df, aes(x, de, colour = method)) +
  geom_line(linewidth = 0.6) +
  scale_colour_brewer(palette = "Dark2", name = NULL) +
  labs(x = "position in data range", y = expression(Delta * E ~ "per step"),
       title = "C. Perceptual uniformity (flat = equal data steps look equal)",
       subtitle = paste0("The contrast-curve line is deliberately non-uniform (peak ",
                         round(max(de_df$de)), ", off scale)")) +
  coord_cartesian(ylim = c(0, 10)) +
  theme_minimal(base_size = 9) +
  theme(legend.position = "bottom", legend.key.width = unit(18, "pt"),
        panel.grid.minor = element_blank()) +
  guides(colour = guide_legend(nrow = 2))

# ---- panel D: what ggplot2 does not do out of the box ------------------

grid <- expand.grid(x = seq(0, 1, length.out = 90), y = seq(0, 1, length.out = 90))
# Smooth, so the clipped regions read as contiguous areas rather than speckle.
grid$z <- with(grid, 2.2 * sin(2.2 * pi * x) * cos(1.7 * pi * y) +
                 0.5 * sin(3.1 * pi * (x + y)))

p_clip <- gradientParams(
  DIV, midpoints = 0.5, width = 0.3,
  low.clip.color = "grey35", high.clip.color = "gold"
)

pD <- ggplot(grid, aes(x, y, fill = z)) +
  geom_raster() +
  asScaleFillGradient(p_clip, limits = c(-2, 2)) +
  coord_cartesian(expand = FALSE) +
  labs(title = "D. Sharpened transition plus two-sided clipping",
       subtitle = "Out-of-range values get their own colours, shown as bands on the colourbar",
       x = NULL, y = NULL, fill = "z") +
  theme_minimal(base_size = 9) +
  theme(axis.text = element_blank(), panel.grid = element_blank(),
        legend.key.width = unit(10, "pt"), legend.key.height = unit(30, "pt"))

# ---- assemble ---------------------------------------------------------

fig <- (pA / pB / pC / pD) +
  plot_layout(heights = c(1, 1, 1, 1.05))

ggsave("man/figures/README-gradients.png", fig,
       width = 7.5, height = 11.5, dpi = 130, bg = "white")

message("wrote man/figures/README-gradients.png")
