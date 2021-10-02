




#' Bin predicted risk for plotting
#'
#' @inheritParams scalib_gnd
#'
#' @param bin_count (_integer value_) total count of bins for downstream plots
#'
#' @param bin_yintercept (_numeric value_) where, relative to the y-axis, the
#'   bins should originate from on downstream plots.
#'
#' @param bin_length (_numeric value_) the length of the bins on downstream
#'   plots.
#'
#' @return a `data.frame` object with values that can be plugged into
#'   standard plotting tools, e.g., `ggplot2::ggplot()` (see examples).
#'
#' @export
#'
#' @examples
#'

predrisk_bin_segments <- function(x,
                                  event_time = NULL,
                                  event_status = NULL,
                                  pred_horizon = NULL,
                                  by_event = FALSE,
                                  bin_count = 100,
                                  bin_yintercept = 0,
                                  bin_length = 1){

  UseMethod("predrisk_bin_segments")

}

#' @export
predrisk_bin_segments.default <- function(x,
                                          event_time = NULL,
                                          event_status = NULL,
                                          pred_horizon = NULL,
                                          by_event = FALSE,
                                          bin_count = 100,
                                          bin_yintercept = 0,
                                          bin_length = 1){

  stop("predrisk_bin_segments does not support objects of class ",
       paste_collapse(class(x)),
       call. = FALSE)

}
#' @export
predrisk_bin_segments.numeric <- function(x,
                                          event_time = NULL,
                                          event_status = NULL,
                                          pred_horizon = NULL,
                                          by_event = FALSE,
                                          bin_count = 100,
                                          bin_yintercept = 0,
                                          bin_length = 1){

  check_call(
    match.call(),
    expected = list(
      'x' = list(type = 'numeric', lwr = 0, upr = 1),
      'event_status' = list(type = 'numeric', uni = c(0, 1)),
      'event_time' = list(type = 'numeric', lwr = 0),
      'pred_horizon' = list(type = 'numeric', lwr = 0, length = 1),
      'by_event' = list(type = 'logical', length = 1),
      'bin_count' = list(type = 'numeric', length = 1, lwr = 1, integer = TRUE),
      'bin_yintercept' = list(type = 'numeric', length = 1),
      'bin_length' = list(type = 'numeric', length = 1)
    )
  )

  events_missing <-
    is.null(event_status) || is.null(event_time) || is.null(pred_horizon)

  if(events_missing && by_event)
    stop("cannot stratify by events if pred_horizon, ",
         "event_time, or event_status is missing.",
         call. = FALSE)

  predrisk_bin_segments_(x,
                         event_time = event_time,
                         event_status = event_status,
                         pred_horizon = pred_horizon,
                         by_event = by_event,
                         bin_count = bin_count,
                         bin_yintercept = bin_yintercept,
                         bin_length = bin_length)

}

predrisk_bin_segments_ <- function(x,
                                   event_time,
                                   event_status,
                                   pred_horizon,
                                   by_event,
                                   bin_count,
                                   bin_yintercept,
                                   bin_length){

  bins <- seq(from = min(x),
              to   = max(x),
              length.out = bin_count + 1)

  if(!by_event){

    freqs <- table(cut(x, bins))
    freqs <- 0.1 * freqs / max(freqs)

    return(
      data.frame(
        x = bins[-(bin_count + 1)],
        y = bin_yintercept,
        xend = bins[-(bin_count + 1)],
        yend = as.numeric(bin_length * freqs + bin_yintercept)
      )
    )

  }

  y <- event_status
  y[event_time < pred_horizon] <- 0

  # separate frequency counts by event status
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

#' @export
predrisk_bin_segments.scalib <- function(x,
                                         event_time = NULL,
                                         event_status = NULL,
                                         pred_horizon = NULL,
                                         by_event = FALSE,
                                         bin_count = 100,
                                         bin_yintercept = 0,
                                         bin_length = 1){


  if(!is.null(event_time))
    stop("event_time should be NULL when x is a scalib object",
         call. = FALSE)

  if(!is.null(event_status))
    stop("event_status should be NULL when x is a scalib object",
         call. = FALSE)

  if(!is.null(pred_horizon))
    stop("pred_horizon should be NULL when x is a scalib object",
         call. = FALSE)


  pred_risk_cols <- attr(x, 'pred_risk_cols')

  out <- melt(x$data_inputs,
              id.vars = c('event_time', 'event_status'),
              measure.vars = pred_risk_cols,
              variable.name = '._id_.')

  out <- out[
    ,
    .(
      list(
        predrisk_bin_segments_(
          x = value,
          event_time = event_time,
          event_status = event_status,
          pred_horizon = x$pred_horizon,
          by_event = by_event,
          bin_count = bin_count,
          bin_yintercept = bin_yintercept,
          bin_length = bin_length
        )
      )
    ),
    keyby = ._id_.
  ]

  out[, unlist(V1, recursive = FALSE), by = ._id_.]

}
