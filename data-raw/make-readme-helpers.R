# Generates man/figures/README-helpers.png: what the colour helpers return.
#
#   Rscript data-raw/make-readme-helpers.R

for (pkg in c("ggplot2", "patchwork")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("This script needs the suggested package '", pkg, "'.", call. = FALSE)
  }
}

library(ggplot2)
library(patchwork)
library(brouillet)

dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)

# Discrete swatches, labelled with the call that produced them.
swatches <- function(sets, title, subtitle = NULL) {
  df <- do.call(rbind, lapply(names(sets), function(nm) {
    cols <- sets[[nm]]
    data.frame(label = nm, i = seq_along(cols), fill = cols)
  }))
  df$label <- factor(df$label, levels = rev(names(sets)))

  ggplot(df, aes(i, label, fill = fill)) +
    geom_tile(colour = "white", linewidth = 1.2) +
    scale_fill_identity() +
    scale_x_continuous(expand = c(0, 0)) +
    labs(x = NULL, y = NULL, title = title, subtitle = subtitle) +
    theme_minimal(base_size = 9) +
    theme(axis.text.x = element_blank(), panel.grid = element_blank(),
          axis.text.y = element_text(family = "mono", size = 8, hjust = 1),
          plot.subtitle = element_text(size = 8, colour = "grey35"))
}

pA <- swatches(list(
  'colorWheel(red, 12)' = colorWheel("red", 12),
  'colorWheel(firebrick, 8)' = colorWheel("firebrick", 8),
  'colorWheel(navy, 6)' = colorWheel("navy", 6)
), "colorWheel() \u2014 hues around the wheel",
   "Saturation and value are taken from the base colour and held fixed")

pB <- swatches(list(
  'colorSpread(blue, 3)' = colorSpread("blue", 3),
  'colorSpread(blue, 4)' = colorSpread("blue", 4),
  'colorSpread(blue, 6)' = colorSpread("blue", 6)
), "colorSpread() \u2014 evenly spaced companions",
   "Returns spread - 1 colours; the base colour itself is not included")

pC <- swatches(list(
  'red + complement' = c("red", colorComplementary("red")),
  'navy + complement' = c("navy", colorComplementary("navy")),
  'gold + complement' = c("gold", colorComplementary("gold"))
), "colorComplementary() \u2014 the opposite hue",
   "Base colour on the left, its complement on the right")

shift_series <- function(color, color2) {
  vapply(seq(0, 100, by = 10), function(s) {
    colorUpdate(color, color2, shift = s, resolution = 100)
  }, character(1))
}

pD <- swatches(list(
  'colorUpdate(red, blue)' = shift_series("red", "blue"),
  'colorUpdate(red, NULL)' = shift_series("red", NULL),
  'colorUpdate(red, complementary)' = shift_series("red", "complementary"),
  'colorBlend(red, blue)' = vapply(seq(0, 1, by = 0.1), function(w) {
    colorBlend("red", "blue", weight = w)
  }, character(1))
), "colorUpdate() / colorBlend() \u2014 between two colours",
   paste("shift runs 0 (all color2) to 100 (all color); weight runs the other way.",
         "\ncolorUpdate() interpolates in Lab by default, colorBlend() in sRGB \u2014 hence the difference."))

fig <- (pA / pB / pC / pD) + plot_layout(heights = c(1, 1, 1, 1.25))

ggsave("man/figures/README-helpers.png", fig,
       width = 8, height = 8.5, dpi = 130, bg = "white")

message("wrote man/figures/README-helpers.png")
