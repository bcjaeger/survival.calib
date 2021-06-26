

#' Modified Greenwood-Nam D'Agostino Test
#'
#'
#' @param predicted_risk
#' @param event_time
#' @param event_status
#' @param time_predict
#' @param time_admin_censor
#' @param group_count_init
#' @param group_count_min
#' @param group_min_events
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples

gnd_test_auto <- function(predicted_risk,
                          event_time,
                          event_status,
                          time_predict,
                          time_admin_censor,
                          group_count_init = 10,
                          group_count_min = 2,
                          group_min_events = 5,
                          verbose = 1) {

 do_over <- TRUE
 too_few_groups <- FALSE
 group_count <- group_count_init

 repeat {

  # cut2 imported from Hmisc

  if(verbose > 0)
   message("Checking event counts using ", group_count, " risk groups...",
           appendLF = FALSE)

  group = cut_percentiles(predicted_risk, g = group_count)

  status_before_horizon <- event_status
  status_before_horizon[event_time > time_predict] <- 0

  group_event_table <-
   table(group = group, event_status = status_before_horizon)

  group_event_counts <-
   subset(as.data.frame(group_event_table), event_status == 1)

  if(all(group_event_counts$Freq > group_min_events)){
   if(verbose > 0) message("okay")
   do_over <- FALSE
   break
  }

  group_count <- group_count - 1
  if(verbose > 0) message("too few; trying again")

  if(group_count < group_count_min){
   too_few_groups <- TRUE
   do_over <- FALSE
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

  GND_result <- try(
   gnd_test_manual(predicted_risk = predicted_risk,
                   event_time = event_time,
                   event_status = event_status,
                   group = group,
                   time_admin_censor = time_admin_censor,
                   group_min_events = group_min_events,
                   verbose = verbose - 1),
   silent = TRUE
  )

  if(verbose == 1) message("..Done")

  GND_fail <- inherits(GND_result, 'try-error')

 }

 if(GND_fail){

  GND.chisq <- NA_real_
  GND.pvalue <- NA_real_
  group_count <- NA_real_

 } else {

  GND.chisq <- GND_result['chi2gw']
  GND.pvalue <- GND_result['pvalgw']

 }

 data.frame(
  GND_df = group_count-1,
  GND_chisq = GND.chisq,
  GND_pvalue = GND.pvalue,
  row.names = NULL
 )

}

