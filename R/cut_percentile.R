

# similar to Hmisc::cut2 but simpler
cut_percentiles <- function(x, g){

 probs <- seq(0, 1, length.out = g+1)
 breaks <- quantile(x, probs = probs)
 as.numeric(
  cut(x,
      breaks = breaks,
      right = FALSE,
      labels = seq(g),
      include.lowest = TRUE)
 )

}
