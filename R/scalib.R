
#' Survival calibration objects
#'
#' @param pred_risk (_numeric vector_, _list_, _data frame_, or _matrix_)
#'   predicted risk values for the event at or before `pred_horizon`
#'
#' @param pred_horizon (_numeric value_) the time of risk prediction.
#'
#' @param event_status (_numeric vector_) observed event status. The values
#'   of this vector should be 0 (event censored) and 1 (event observed).
#'
#' @param event_time (_numeric vector_) observed event times
#'
#' @return an object of class 'scalib'
#'
#' @export
#'
#' @examples
#'
#' sc <- scalib(pred_risk = pbc_scalib$predrisk,
#'              pred_horizon = 2500,
#'              event_time = pbc_scalib$test$time,
#'              event_status = pbc_scalib$test$status)
#'
#' print(sc)

scalib <- function(pred_risk,
                   pred_horizon,
                   event_status,
                   event_time){

  if(any(is.na(event_time)))
    stop("missing values in event_time", call. = FALSE)

  if(any(is.na(event_status)))
    stop("missing values in event_status", call. = FALSE)

  check_call(
    match.call(),
    expected = list(
      'pred_horizon' = list(type = 'numeric', lwr = 0, length = 1),
      'event_status' = list(type = 'numeric', uni = c(0, 1)),
      'event_time' = list(type = 'numeric', lwr = 0)
    )
  )

  scalib_new(pred_risk,
             pred_horizon,
             event_status,
             event_time)

}

is_scalib <- function(x){
  inherits(x, "scalib")
}


scalib_new <- function(pred_risk,
                       pred_horizon,
                       event_status,
                       event_time){

  data_inputs <- make_scalib_input_data(
    pred_risk = pred_risk,
    event_status = event_status,
    event_time = event_time
  )

  pred_risk_cols <-
    setdiff(names(data_inputs), c('event_time', 'event_status'))

  preds_on_boundaries <- data_inputs[
    , sapply(.SD, function(x) min(x)==0 | max(x)==1),
    .SDcols = pred_risk_cols
  ]

  if(any(preds_on_boundaries)){

    warning("Some predicted risk values are either 1 or 0.",
            "\nThese have been changed to 0.9999 or 0.0001",
            call. = FALSE)

    data_inputs[
      , (pred_risk_cols) := lapply(.SD, boundary_enforce),
      .SDcols = pred_risk_cols
    ]

  }

  structure(
    .Data = list(
      pred_horizon = pred_horizon,
      data_inputs = data_inputs,
      data_outputs = data.table()
    ),
    class = 'scalib',
    pred_risk_cols = pred_risk_cols
  )


}

make_scalib_input_data <- function(pred_risk,
                                   event_status,
                                   event_time){

  UseMethod('make_scalib_input_data')

}

make_scalib_input_data.default <- function(pred_risk,
                                           event_status,
                                           event_time){

  .class <- class(pred_risk)[1]

  stop("No applicable method for 'make_scalib_input_data' that can be",
       "\napplied to an object of class ", .class, call. = FALSE)

}

make_scalib_input_data.numeric <- function(pred_risk,
                                           event_status,
                                           event_time){

  if(any(is.na(pred_risk)))
    stop("missing values in pred_risk", call. = FALSE)

  stopifnot(length(pred_risk) == length(event_status))
  stopifnot(length(pred_risk) == length(event_time))

  check_arg_bounds(pred_risk,
                   arg_name = 'pred_risk',
                   bound_lwr = 0,
                   bound_upr = 1)

  data.table::data.table(
    pred_risk = pred_risk,
    event_status = event_status,
    event_time = event_time
  )

}

make_scalib_input_data.list <- function(pred_risk,
                                        event_status,
                                        event_time){


  for(i in seq_along(pred_risk)){

    if(any(is.na(pred_risk[[i]])))
      stop("missing values in pred_risk", call. = FALSE)

    check_arg_bounds(pred_risk[[i]],
                     arg_name = 'pred_risk',
                     bound_lwr = 0,
                     bound_upr = 1)

    check_arg_type(arg_value = pred_risk[[i]],
                   arg_name = 'pred_risk',
                   expected_type = 'numeric')

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


  if(any(is.na(pred_risk)))
    stop("missing values in pred_risk", call. = FALSE)

  check_arg_type(arg_value = pred_risk,
                 arg_name = 'pred_risk',
                 expected_type = 'numeric')

  check_arg_bounds(pred_risk,
                   arg_name = 'pred_risk',
                   bound_lwr = 0,
                   bound_upr = 1)

  if(is.null(colnames(pred_risk))){
    colnames(pred_risk) <- paste0("pred_risk_", seq(ncol(pred_risk)))
  }

  if("" %in% colnames(pred_risk))
    stop("pred_risk matrix has blank column names.",
         call. = FALSE)

  dt <- data.table::data.table(
    event_time = event_time,
    event_status = event_status
  )

  cbind(dt, pred_risk)

}

make_scalib_input_data.data.frame <- function(pred_risk,
                                              event_status,
                                              event_time){

  for(i in seq(ncol(pred_risk))){

    if(any(is.na(pred_risk[[i]])))
      stop("missing values in pred_risk", call. = FALSE)

    check_arg_bounds(
      pred_risk[[i]],
      arg_name = paste('pred_risk', names(pred_risk)[i], sep = '$'),
      bound_lwr = 0,
      bound_upr = 1
    )

    check_arg_type(
      arg_value = pred_risk[[i]],
      arg_name = paste('pred_risk', names(pred_risk)[i], sep = '$'),
      expected_type = 'numeric'
    )

  }

  dt <- data.table::data.table(
    event_time = event_time,
    event_status = event_status
  )

  cbind(dt, pred_risk)

}



