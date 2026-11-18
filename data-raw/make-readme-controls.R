# Generates man/figures/README-controls.png: what each knob actually does.
#
#   Rscript data-raw/make-readme-controls.R

for (pkg in c("ggplot2", "patchwork")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("This script needs the suggested package '", pkg, "'.", call. = FALSE)
  }
}

library(ggplot2)
library(patchwork)
library(brouillet)

dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)

COLS <- c("navy", "white", "firebrick")

# Each strip is drawn with asColorFun(), so the x axis is in data units and
# out-of-range values show whatever the specification says they should.
strip_data <- function(specs, from = 0, to = 1, n = 400) {
  x <- seq(from, to, length.out = n)
  do.call(rbind, lapply(names(specs), function(nm) {
    data.frame(x = x, label = nm,
               fill = asColorFun(specs[[nm]], limits = c(0, 1))(x))
  }))
}

strip_plot <- function(df, title, subtitle = NULL) {
  df$label <- factor(df$label, levels = rev(unique(df$label)))
  ggplot(df, aes(x, 1, fill = fill)) +
    geom_raster() +
    scale_fill_identity() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(NULL, breaks = NULL, expand = c(0, 0)) +
    facet_wrap(~label, ncol = 1, strip.position = "left") +
    labs(x = NULL, title = title, subtitle = subtitle) +
    theme_minimal(base_size = 9) +
    theme(strip.text.y.left = element_text(angle = 0, hjust = 1, size = 8,
                                           family = "mono"),
          panel.spacing = unit(2, "pt"), panel.grid = element_blank(),
          plot.subtitle = element_text(size = 8, colour = "grey35"))
}

spec <- function(...) gradientParams(COLS, midpoints = 0.5, ...)

# --- width: how much of the range the colour change spans -------------------
pA <- strip_plot(strip_data(list(
  "width = 1.0" = spec(width = 1),
  "width = 0.6" = spec(width = 0.6),
  "width = 0.4" = spec(width = 0.4),
  "width = 0.2" = spec(width = 0.2),
  "width = 0.1" = spec(width = 0.1)
)), "width \u2014 how sharp the transition is",
   "Small values compress the middle and expand the extremes")

# --- center: where the change happens ---------------------------------------
pB <- strip_plot(strip_data(list(
  "center = 0.20" = spec(center = 0.2, width = 0.3),
  "center = 0.35" = spec(center = 0.35, width = 0.3),
  "center = 0.50" = spec(center = 0.5, width = 0.3),
  "center = 0.65" = spec(center = 0.65, width = 0.3),
  "center = 0.80" = spec(center = 0.8, width = 0.3)
)), "center \u2014 where the change happens",
   "Within each segment: the 50% point of the curve, not the colour stop")

# --- asymmetry: which side rolls off slowly ---------------------------------
pC <- strip_plot(strip_data(list(
  "asymmetry = -0.8" = spec(asymmetry = -0.8),
  "asymmetry = -0.4" = spec(asymmetry = -0.4),
  "asymmetry =  0.0" = spec(asymmetry = 0),
  "asymmetry = +0.4" = spec(asymmetry = 0.4),
  "asymmetry = +0.8" = spec(asymmetry = 0.8)
)), "asymmetry \u2014 which side rolls off slowly",
   "Positive: sharp below center, gradual above (long upper tail)")

# --- clipping ---------------------------------------------------------------
pD <- strip_plot(strip_data(list(
  "no clipping" = spec(width = 0.4),
  "hard clip" = spec(width = 0.4, low.clip.color = "grey35",
                     high.clip.color = "gold"),
  "blend.clip = TRUE" = spec(width = 0.4, low.clip.color = "grey35",
                             high.clip.color = "gold", blend.clip = TRUE),
  "high side only" = spec(width = 0.4, high.clip.color = "gold")
), from = -0.25, to = 1.25),
  "clipping \u2014 colours for out-of-range values",
  "Drawn from -0.25 to 1.25 with limits = c(0, 1); the ends are out of range")

# --- multi-stop: any number of colours, length(colors) - 2 midpoints --------
MULTI <- c("black", "navy", "white", "firebrick", "gold")

pE <- strip_plot(strip_data(list(
  "4 colours, even" = gradientParams(MULTI[-1], width = 0.4),
  "5 colours, even" = gradientParams(MULTI, width = 0.4),
  "5, mids .2/.5/.75" = gradientParams(MULTI, midpoints = c(0.2, 0.5, 0.75),
                                        width = 0.4),
  "5, mids .5/.7/.85" = gradientParams(MULTI, midpoints = c(0.5, 0.7, 0.85),
                                        width = 0.4),
  "5, width = 0.15" = gradientParams(MULTI, midpoints = c(0.2, 0.5, 0.75),
                                      width = 0.15)
)), "multi-stop \u2014 any number of colours",
   "The curve applies within each segment, so a narrow width posterises")

fig <- (pA / pB / pC / pD / pE)

ggsave("man/figures/README-controls.png", fig,
       width = 7, height = 12.2, dpi = 130, bg = "white")

message("wrote man/figures/README-controls.png")
