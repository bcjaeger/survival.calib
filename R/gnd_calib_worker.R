
gnd_calib_worker <- function(data,
                             time_predict,
                             group_min_events_warn,
                             group_min_events_stop) {

 if (sum(data$event_status) < group_min_events_stop)
  return(c(0, 0, 0, 0, stopped = -1))

 stopped <- as.numeric(sum(data$event_status) < group_min_events_warn)

 .surv <- survival::survfit(
  formula = survival::Surv(event_time, event_status) ~ 1,
  data = data,
  se.fit = TRUE
 )

 # initialize survival as 1; standard error as 0; number of events as 0
 est <- 1; stderr <- 0; num <- 0

 # if there is an event observed before admin censor time, then
 # update survival estimate, standard error, and number of events
 if(min(.surv$time) <= time_predict){

  # find the last event time before censoring
  i <- which.max(.surv$time[.surv$time <= time_predict])
  est    <- .surv$surv[i]
  stderr <- .surv$std.err[i]
  num    <- .surv$n.risk[i]

 }

 c(est     = est,
   stderr  = stderr * est,
   num     = num,
   stopped = stopped)

}


