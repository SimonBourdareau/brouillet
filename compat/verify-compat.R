suppressMessages({library(brouillet); library(ggplot2)})
orig <- new.env(); sys.source("original.R", orig)
source("brouillet-compat.R")

maxd <- function(a, b) {
  if (length(a) != length(b)) return(NA_real_)
  max(sqrt(rowSums((t(col2rgb(a)) - t(col2rgb(b)))^2)))
}

cases <- list(
  list("sigmoid_gradient2 low", quote(sigmoid_gradient2("red","blue", steepness_preset="low"))),
  list("sigmoid_gradient2 extreme", quote(sigmoid_gradient2("navy","firebrick", steepness_preset="extreme"))),
  list("sigmoid_gradient2 sigmas", quote(sigmoid_gradient2("red","blue", sigma1=80, sigma2=80, inflexpoint1=.2, inflexpoint2=.8))),
  list("sigmoid_gradient2 longtail", quote(sigmoid_gradient2("white","darkgreen", steepness_preset="extreme_longtail"))),
  list("sigmoid_gradient2 limits", quote(sigmoid_gradient2("red","blue", steepness_preset="low", low.value=-5, high.value=5))),
  list("sigmoid_gradient3 default", quote(sigmoid_gradient3("red","blue","white", steepness_preset="low"))),
  list("sigmoid_gradient3 mid .25", quote(sigmoid_gradient3("red","blue","white", steepness_preset="medium", midpoint.value=.25))),
  list("sigmoid_gradient3 mid .75", quote(sigmoid_gradient3("navy","gold","white", steepness_preset="extreme", midpoint.value=.75))),
  list("sigmoid_gradientn 4col", quote(sigmoid_gradientn(c("red","white","blue","green"), c(0.3,0.6), steepness_preset="low"))),
  list("sigmoid_gradientn 5col", quote(sigmoid_gradientn(c("black","navy","white","firebrick","gold"), c(.2,.5,.75), steepness_preset="medium"))),
  list("linear_gradient2", quote(linear_gradient2("red","blue"))),
  list("linear_gradient2 inflex", quote(linear_gradient2("red","blue", inflexpoint1=.2, inflexpoint2=.8))),
  list("linear_gradientn 3col", quote(linear_gradientn(c("red","white","blue"), 0.5))),
  list("linear_gradientn limits", quote(linear_gradientn(c("red","white","blue"), 0, low.value=-5, high.value=5))),
  list("custom_gradient_fun x^2", quote(custom_gradient_fun(c("blue","red"), function(x) x^2))),
  list("custom_gradient_fun sqrt", quote(custom_gradient_fun(c("white","navy"), sqrt)))
)

cat(sprintf("%-30s %6s %8s %8s\n", "call", "rows", "meanDist", "maxDist"))
cat(strrep("-", 56), "\n")
worst <- 0
for (cs in cases) {
  o <- eval(cs[[2]], orig)
  n <- eval(cs[[2]], globalenv())
  d <- sqrt(rowSums((t(col2rgb(o$color)) - t(col2rgb(n$color)))^2))
  worst <- max(worst, max(d))
  cat(sprintf("%-30s %6d %8.2f %8.1f\n", cs[[1]], nrow(n), mean(d), max(d)))
}
cat(strrep("-", 56), "\n")
cat(sprintf("worst deviation across all cases: %.1f RGB units (of 441 max)\n", worst))
