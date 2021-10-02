



#' @rdname scalib_gnd
#' @export
vec_gnd <- function(pred_risk,
                    pred_horizon,
                    event_status,
                    event_time,
                    group_method = 'lump',
                    group_count_init = 10,
                    group_count_min = 2,
                    group_min_events = 5,
                    verbose = 0){

  .scalib <- scalib(
    pred_risk = pred_risk,
    pred_horizon = pred_horizon,
    event_status = event_status,
    event_time = event_time
  )

  .out <- scalib_gnd(
    scalib_object = .scalib,
    group_method = group_method,
    group_count_init = group_count_init,
    group_count_min = group_count_min,
    group_min_events = group_min_events,
    verbose = verbose
  )

  getElement(.out, "data_outputs")

}

#' @rdname scalib_gnd
#' @export
vec_gnd_manual <- function(pred_risk,
                           pred_horizon,
                           event_status,
                           event_time,
                           group,
                           group_min_events_warn = 5,
                           group_min_events_stop = 2,
                           verbose = 0){

  .scalib <- scalib(
    pred_risk = pred_risk,
    pred_horizon = pred_horizon,
    event_status = event_status,
    event_time = event_time
  )

  .out <- scalib_gnd_manual(
    scalib_object = .scalib,
    group = group,
    pred_risk_col = attr(.scalib, 'pred_risk_cols')[1],
    group_min_events_warn = group_min_events_warn,
    group_min_events_stop = group_min_events_stop,
    verbose = verbose
  )

  getElement(.out, "data_outputs")

}
