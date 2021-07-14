

#' Cut data into percentile groups
#'
#' `predrisk_grp_prcnt()` is a simple wrapper for `cut` that is meant to be used
#'   to create an input value for [calib_test_gnd_manual()]  (specifically, the
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
#' predrisk_grp_prcnt(sort(rnorm(100)), g = 10)
#'
predrisk_grp_prcnt <- function(x,
                               g,
                               na.rm = FALSE,
                               type = 7){

  check_call(
    match.call(),
    expected = list(
      'x' = list(type = 'numeric', lwr = 0, upr = 1),
      'g' = list(type = 'numeric', integer = TRUE, length = 1),
      'na.rm' = list(type = 'logical', length = 1),
      'type' = list(type = 'numeric', integer = TRUE, lwr = 1, upr = 9)
    )
  )

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
