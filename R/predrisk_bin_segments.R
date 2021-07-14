




#' Bin predicted risk for plotting
#'
#' @inheritParams predrisk_grp_prcnt
#' @inheritParams calib_test_gnd
#' @param x_min (_numeric value_) minimum value of x-axis on downstream plots.
#' @param x_max (_numeric value_) maximum value of x-axis on downstream plots.
#' @param bin_count (_integer value_) total count of bins for downstream plots
#' @param bin_yintercept (_numeric value_) where, relative to the y-axis, the
#'   bins should originate from on downstream plots.
#' @param bin_length (_numeric value_) the length of the bins on downstream
#'   plots.
#'
#' @return `data.frame` object with values that can be plugged into
#'   standard plotting tools, e.g., `ggplot2::ggplot()` (see examples).
#'
#' @export
#'
#' @examples
#'
#'
#'
predrisk_bin_segments <- function(x,
                                  event_status,
                                  event_time,
                                  time_predict,
                                  x_min = 0,
                                  x_max = 1,
                                  bin_count = 100,
                                  bin_yintercept = 0,
                                  bin_length = 1){

  check_call(
    match.call(),
    expected = list(
      'x' = list(type = 'numeric', lwr = 0, upr = 1),
      'event_status' = list(type = 'numeric', uni = c(0, 1)),
      'event_time' = list(type = 'numeric', lwr = 0),
      'time_predict' = list(type = 'numeric', lwr = 0, length = 1),
      'x_min' = list(type = 'numeric', lwr = 0, upr = 1),
      'x_max' = list(type = 'numeric', lwr = 0, upr = 1),
      'bin_count' = list(type = 'numeric', lwr = 1, integer = TRUE),
      'bin_yintercept' = list(type = 'numeric'),
      'bin_length' = list(type = 'numeric')
    )
  )

  y <- event_status
  y[event_time > time_predict] <- 0

  bins <- seq(from = x_min,
              to = x_max,
              length.out = bin_count + 1)

  x[x < x_min] <- x_min * 1.001
  x[x > x_max] <- x_max * 0.999

  f0	<- table(cut(x[y == 0], bins))
  f1	<- table(cut(x[y == 1], bins))

  j0	<- f0 > 0
  j1	<- f1 > 0

  bins0 <- (bins[-(bin_count + 1)])[j0]
  bins1 <- (bins[-(bin_count + 1)])[j1]

  f0	<- f0[j0]
  f1	<- f1[j1]

  maxf <- max(f0, f1)

  f0	<- (0.1 * f0) / maxf
  f1	<- (0.1 * f1) / maxf

  data_segments_below <- data.frame(
    x = bins0,
    y = bin_yintercept,
    xend = bins0,
    yend = as.numeric(-1 * bin_length * f0 + bin_yintercept),
    event_status = 0
  )

  data_segments_above <- data.frame(
    x = bins1,
    y = bin_yintercept,
    xend = bins1,
    yend = as.numeric(bin_length * f1 + bin_yintercept),
    event_status = 1
  )

  rbind(data_segments_above, data_segments_below)

}
