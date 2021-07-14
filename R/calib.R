

calib_new <- function(pred_risk,
                      pred_horizon,
                      event_status,
                      event_time){


 # observed event status and times, curtailed at prediction horizon
 .event_status = ifelse(test = event_time > pred_horizon,
                        yes = 0,
                        no = event_status)

 .event_time = pmin(event_time, pred_horizon)

 structure(
  .Data = list(pred_risk = pred_risk,
               pred_horizon = pred_horizon,
               event_status = .event_status,
               event_time = .event_time,
               data = data.table::data.table()),
  class = 'survival_calib'
 )


}


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
calib_initiate <- function(pred_risk,
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

 calib_new(pred_risk,
           pred_horizon,
           event_status,
           event_time)

}


