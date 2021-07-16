#' Title
#'
#' @param pred_risk
#' @param pred_horizon
#' @param event_status
#' @param event_time
#'
#' @return
#' @export
#'
#' @examples
scalib_initiate <- function(pred_risk,
                            pred_horizon,
                            event_status,
                            event_time){

  check_call(
    match.call(),
    expected = list(
      'pred_horizon' = list(type = 'numeric', lwr = 0),
      'event_status' = list(type = 'numeric', uni = c(0, 1)),
      'event_time' = list(type = 'numeric', lwr = 0)
    )
  )

  scalib_new(pred_risk,
             pred_horizon,
             event_status,
             event_time)

}



scalib_new <- function(pred_risk,
                       pred_horizon,
                       event_status,
                       event_time){

  # observed event status and times, curtailed at prediction horizon
  .event_status = ifelse(test = event_time > pred_horizon,
                         yes = 0,
                         no = event_status)

  .event_time = pmin(event_time, pred_horizon)

  structure(
    .Data = list(
      pred_horizon = pred_horizon,
      data_inputs = make_scalib_input_data(
        pred_risk = pred_risk,
        event_status = .event_status,
        event_time = .event_time
      ),
      data_outputs = data.table::data.table()
    ),
    class = 'survival_calib'
  )


}

make_scalib_input_data <- function(pred_risk,
                                   event_status,
                                   event_time){

  UseMethod('make_scalib_input_data')

}

make_scalib_input_data.numeric <- function(pred_risk,
                                           event_status,
                                           event_time){

  data.table::data.table(
    pred_risk = pred_risk,
    event_status = event_status,
    event_time = event_time
  )

}

make_scalib_input_data.list <- function(pred_risk,
                                        event_status,
                                        event_time){

  if(length(pred_risk) == 1){
    return(
      make_scalib_input_data(
        as.numeric(pred_risk[[1]]),
        event_status,
        event_time
      )
    )
  }

  if(is.null(names(pred_risk))){
    names(pred_risk) <- paste0("pred_risk_", seq(length(pred_risk)))
  }

  dt <- data.table::data.table(
    event_time = event_time,
    event_status = event_status
  )

  cbind(dt, data.table::as.data.table(pred_risk))

}

make_scalib_input_data.matrix <- function(pred_risk,
                                          event_status,
                                          event_time){

  if(ncol(pred_risk) == 1){
    return(
      make_scalib_input_data(
        as.numeric(pred_risk),
        event_status,
        event_time
      )
    )
  }

  if(is.null(colnames(pred_risk))){
    colnames(pred_risk) <- paste0("pred_risk_", seq(ncol(pred_risk)))
  }

  dt <- data.table::data.table(
    event_time = event_time,
    event_status = event_status
  )

  cbind(dt, pred_risk)

}

make_scalib_input_data.data.frame <- function(pred_risk,
                                              event_status,
                                              event_time){

  names(pred_risk) <- paste0("pred_risk_", names(pred_risk))

  dt <- data.table::data.table(
    event_time = event_time,
    event_status = event_status
  )

  cbind(dt, pred_risk)

}



