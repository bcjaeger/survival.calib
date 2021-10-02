

#' Cut data into percentile groups
#'
#' `predrisk_grp_prcnt()` is a simple wrapper for `cut` that is meant to be used
#'   to create an input value for [scalib_gnd_manual()]  (specifically, the
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
                           type = type)

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



# deprecated
# predrisk_grp_gnd <- function(x,
#                              event_status,
#                              group_method,
#                              group_count_init,
#                              group_count_min,
#                              group_min_events_warn,
#                              group_min_events_stop,
#                              verbose){
#
#  too_few_groups <- FALSE
#  group_count <- group_count_init
#
#  repeat {
#
#   if(verbose > 0)
#    message("Checking event counts using ",
#            group_count, " risk groups...",
#            appendLF = FALSE)
#
#   if(group_count == group_count_init){
#    group = predrisk_grp_prcnt(
#     x = x,
#     g = group_count
#    )
#   }
#
#   if(group_count < group_count_init){
#
#    group = switch(
#     group_method,
#     "redo" = predrisk_grp_prcnt(
#      x = x,
#      g = group_count
#     ),
#     "lump" = lump_group(
#      group = group,
#      events = group_event_counts,
#      min_size = group_min_events_warn
#     )
#    )
#
#
#   }
#
#   group_event_table <-
#    table(group = group,
#          event_status = event_status)
#
#   group_event_counts <-
#    subset(
#     as.data.frame(group_event_table, stringsAsFactors = FALSE),
#     event_status == 1
#    )
#
#   # Seems silly, but this is needed to keep the groups from getting
#   # character sorted; i.e., 1, 10, 11, ... 19, 2, 20, etc.
#   group_event_counts$group <- as.numeric(group_event_counts$group)
#
#   if(all(group_event_counts$Freq > group_min_events_warn)){
#    if(verbose > 0) message("okay", appendLF = TRUE)
#    break
#   }
#
#   group_count <- group_count - 1
#   if(verbose > 0) message("too few; trying again", appendLF = TRUE)
#
#   if(group_count < group_count_min){
#
#    stop("There are too few events to run the GND test ",
#         "using ", group_count_min, " groups.",
#         call. = FALSE)
#
#   }
#
#  }
#
#  if(verbose > 0)
#   message("Attempting GND test with ", group_count, " risk groups.",
#           appendLF = TRUE)
#
#  group
#
# }
