suppressMessages(library(brouillet))
orig <- new.env(); sys.source("original.R", orig); sys.source("original-params.R", orig)

cases <- list(
  list("2 col, low",        list(colors=c("red","blue"), steepness_preset="low"), c(0,1)),
  list("2 col, extreme",    list(colors=c("navy","firebrick"), steepness_preset="extreme"), c(0,1)),
  list("2 col, preset wins",list(colors=c("red","blue"), sigma1=80, sigma2=80, inflexpoint1=.2, inflexpoint2=.8), c(0,1)),
  list("2 col, sigmas only",list(colors=c("red","blue"), sigma1=80, sigma2=80, steepness_preset=NULL), c(0,1)),
  list("2 col, longtail",   list(colors=c("white","darkgreen"), steepness_preset="extreme_longtail"), c(0,1)),
  list("2 col, limits",     list(colors=c("red","blue"), steepness_preset="low"), c(-5,5)),
  list("2 col, res 250",    list(colors=c("red","blue"), steepness_preset="low", resolution=250), c(0,1)),
  list("3 col, mid .5",     list(colors=c("red","white","blue"), midpoints=0.5, steepness_preset="low"), c(0,1)),
  list("3 col, mid .25",    list(colors=c("red","white","blue"), midpoints=0.25, steepness_preset="medium"), c(0,1)),
  list("3 col, mid .75",    list(colors=c("navy","gold","white"), midpoints=0.75, steepness_preset="extreme"), c(0,1)),
  list("3 col, limits",     list(colors=c("navy","white","gold"), midpoints=0, steepness_preset="high"), c(-2,2)),
  list("3 col, no midpoint",list(colors=c("red","white","blue"), steepness_preset="low"), c(0,1)),
  list("4 col",             list(colors=c("red","white","blue","green"), midpoints=c(0.3,0.6), steepness_preset="low"), c(0,1)),
  list("5 col",             list(colors=c("black","navy","white","firebrick","gold"), midpoints=c(.2,.5,.75), steepness_preset="medium"), c(0,1)),
  list("linear 2",          list(colors=c("red","blue"), gradient_mode="linear"), c(0,1)),
  list("linear 2, inflex",  list(colors=c("red","blue"), gradient_mode="linear", inflexpoint1=.2, inflexpoint2=.8), c(0,1)),
  list("linear 3",          list(colors=c("red","white","blue"), midpoints=0.5, gradient_mode="linear"), c(0,1)),
  list("custom x^2",        list(colors=c("blue","red"), gradient_mode="custom", mapping_fun=function(x) x^2), c(0,1)),
  list("clipping",          list(colors=c("red","blue"), steepness_preset="low", low.clip.color="grey", high.clip.color="black"), c(0,1)),
  list("blend.clip",        list(colors=c("red","blue"), steepness_preset="low", low.clip.color="grey", high.clip.color="black", blend.clip=TRUE), c(0,1))
)

cat(sprintf("%-20s %6s %7s %6s\n", "case", "rows", "meanD", "maxD"))
cat(strrep("-", 43), "\n")
worst <- 0
for (cs in cases) {
  go <- do.call(orig$.build_gradient, list(do.call(orig$gradient_params, cs[[2]]), cs[[3]][1], cs[[3]][2]))
  gn <- buildGradient(do.call(gradient_params, cs[[2]]), limits = cs[[3]])
  if (nrow(go) != nrow(gn)) {
    cat(sprintf("%-20s ROW MISMATCH %d vs %d\n", cs[[1]], nrow(go), nrow(gn))); next
  }
  d <- sqrt(rowSums((t(col2rgb(go$color)) - t(col2rgb(gn$color)))^2))
  worst <- max(worst, max(d))
  cat(sprintf("%-20s %6d %7.2f %6.1f\n", cs[[1]], nrow(gn), mean(d), max(d)))
}
cat(strrep("-", 43), "\n")
cat("worst deviation:", round(worst, 1), "RGB units of a possible 441\n")
