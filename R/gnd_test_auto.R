

#' Modified Greenwood-Nam D'Agostino Test
#'
#' @inheritParams gnd_test_manual
#' @param group_method (_character value_; 'lump' or 'redo') If 'lump', then
#'   a 'lumping' procedure will be applied whenever a group has less than
#'   `group_min_events_warn` events. The lumping procedure will identify
#'   whichever group has the lowest event count and assign members of that
#'   group to the
#' @param group_count_init
#' @param group_count_min
#'
#' @return an object of class 'survival.calib_gnd_test'
#'
#' @export
#'
#' @examples

gnd_test_auto <- function(predicted_risk,
                          event_time,
                          event_status,
                          time_predict,
                          group_method = 'lump',
                          group_count_init = 10,
                          group_count_min = 2,
                          group_min_events_warn = 5,
                          group_min_events_stop = 2,
                          verbose = 1) {

  check_call(
    match.call(),
    expected = list(
      'predicted_risk' = list(type = 'numeric', lwr = 0, upr = 1),
      'event_time' = list(type = 'numeric', lwr = 0),
      'event_status' = list(type = 'numeric', uni = c(0, 1)),
      'time_predict' = list(type = 'numeric', lwr = 0, length = 1),
      'group_count_init' = list(type = 'numeric', length = 1, lwr = 2),
      'group_count_min' = list(type = 'numeric', length = 1, lwr = 2),
      'group_min_events_warn' = list(type = 'numeric', length = 1, lwr = 5),
      'group_min_events_stop' = list(type = 'numeric', length = 1, lwr = 2),
      'verbose' = list(type = 'numeric', length = 1, lwr = 0)
    )
  )

  status_before_horizon <- event_status
  status_before_horizon[event_time > time_predict] <- 0

  too_few_groups <- FALSE
  group_count <- group_count_init

  repeat {

    if(verbose > 0)
      message("Checking event counts using ", group_count, " risk groups...",
              appendLF = FALSE)

    if(group_count == group_count_init){
      group = cut_percentiles(predicted_risk, g = group_count)
    }

    if(group_count < group_count_init){

      group = switch(
        group_method,
        "redo" = cut_percentiles(predicted_risk, g = group_count),
        "lump" = lump_group(group = group,
                            events = group_event_counts,
                            min_size = group_min_events_warn)
      )


    }

    group_event_table <-
      table(group = group, event_status = status_before_horizon)

    group_event_counts <-
      subset(
        as.data.frame(group_event_table, stringsAsFactors = FALSE),
        event_status == 1
      )

    # Seems silly, but this is needed to keep the groups from getting
    # character sorted; i.e., 1, 10, 11, ... 19, 2, 20, etc.
    group_event_counts$group <- as.numeric(group_event_counts$group)

    if(all(group_event_counts$Freq > group_min_events_warn)){
      if(verbose > 0) message("okay")
      break
    }

    group_count <- group_count - 1
    if(verbose > 0) message("too few; trying again")

    if(group_count < group_count_min){
      too_few_groups <- TRUE
      break
    }

  }

  if(too_few_groups){

    GND_fail <- TRUE
    GND_result <- NULL


  } else {

    if(verbose > 0)
      message("Attempting GND test with ", group_count, " risk groups.",
              appendLF = verbose > 1)

    GND_result <- gnd_test_manual(
      predicted_risk = predicted_risk,
      event_time = event_time,
      event_status = event_status,
      group = group,
      time_predict = time_predict,
      group_min_events_warn = group_min_events_warn,
      group_min_events_stop = group_min_events_stop,
      verbose = max(0, verbose - 1)
    )

    if(verbose == 1) message("..Done")

  }

  GND_result

}

