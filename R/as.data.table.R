


#' Coerce to `data.table`
#'
#' Convert `scalib` object into a flat data.table containing
#'   input and output data.
#'
#' @param x An object of class `scalib` (see [scalib]).
#'
#' @param ... currently not used
#'
#' @return a [data.table][data.table::data.table()] object
#'
#' @export
#'
#' @examples
#'
#'
#'  library(data.table)
#'
#'  sc <- scalib(pred_risk = pbc_scalib$predrisk,
#'               pred_horizon = 2500,
#'               event_time = pbc_scalib$test$time,
#'               event_status = pbc_scalib$test$status)
#'
#'  print(sc)
#'
#'  as.data.table(sc)
#'
#'  sc_gnd <- scalib_gnd(sc)
#'
#'  as.data.table(sc_gnd)
#'
#'
as.data.table.scalib <- function(x, ...){

 # good ole' CRAN
 . = NULL
 ._id_. = NULL
 pred_risk = NULL
 event_time = NULL
 event_status = NULL

 dt_inputs <- melt(x$data_inputs,
                   measure.vars = attr(x, 'pred_risk_cols'),
                   id.vars = c('event_time', 'event_status'),
                   variable.name = '._id_.',
                   value.name = 'pred_risk')

 dt_inputs <- dt_inputs[
  ,
  .(
   pred_horizon = x$pred_horizon,
   inputs = list(data.table(event_time, event_status, pred_risk))
  ),
  by = ._id_.
 ]

 if(is_empty(x$data_outputs)) return(dt_inputs)

 dt_outputs <- x$data_outputs[
  ,
  .(outputs = list(data.table(.SD))),
  by = ._id_.
 ]


 dt_inputs[dt_outputs, on = '._id_.']

}
