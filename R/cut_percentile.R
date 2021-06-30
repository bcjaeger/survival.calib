

#' Cut data into percentile groups
#'
#' `cut_percentiles()` is a simple wrapper for `cut` that is meant to be used
#'   to create an input value for [gnd_test_manual()]  (specifically, the
#'   `group` input argument).
#'
#' @param x (_numeric vector_) a numeric vector which will be converted to
#'   a vector of integers by cutting.
#'
#' @param g (_numeric value_) the desired number of percentile groups. For
#'  example, `g = 10` gives decile groups.
#'
#' @param na.rm (_logical value_) if `TRUE`, any `NA` and `NaN` values
#'   are removed from `x` before the quantiles are computed.
#'
#' @param type (_integer value between 1 and 9_) see [quantile][stats::quantile()] for
#'   details.
#'
#' @return a numeric vector with integer values.
#'
#' @export
#'
#' @examples
#'
#' cut_percentiles(sort(rnorm(100)), g = 10)
#'
cut_percentiles <- function(x, g, na.rm = FALSE, type = 7){

 probs <- seq(0, 1, length.out = g+1)
 breaks <- stats::quantile(x,
                           probs = probs,
                           na.rm = na.rm,
                           type = type,
 )

 if(length(unique(breaks)) < length(breaks))
   stop("the number of groups, g, is > ",
        "the number of unique values in x.
       Try reducing g such that g < length(unique(x))",
       call. = FALSE)

 as.numeric(
  cut(x,
      breaks = breaks,
      right = FALSE,
      labels = seq(g),
      include.lowest = TRUE)
 )

}
