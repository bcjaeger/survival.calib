

#' Greenwood-Nam-D'Agostino (GND) Test
#'
#'
#' Demler et al developed a modification of the Nam-D'Agostino test that has
#'   nominal type 1 error rates with censored outcomes. The
#'   Greenwood-Nam-D'Agostino (GND) test is applicable in settings where
#'   the proportional hazards assumption is invalid. The GND test is based
#'   on groupwise error rates for observed versus expected risk, and the
#'   test is unstable when any group contains < 5 events (see Demler et al.).
#'
#' @param scalib_object An object of class `scalib`
#'   (see [scalib]).
#'
#' @inheritParams scalib
#'
#' @param pred_risk_col (_character value_) the column name of the variable
#'   that contains predicted risk values. This variable should be in
#'   `scalib_object$data_inputs`.
#'
#' @param group_method (_character value_; 'lump' or 'redo') If 'lump', then
#'   a 'lumping' procedure will be applied whenever a group has less than
#'   `group_min_events_warn` events. The lumping procedure will identify
#'   whichever group has the lowest event count and assign members of that
#'   group to the group with too few events. If 'redo', then the groups
#'   will be re-done using [predrisk_grp_prcnt] but with one less group.
#'   'lump' is the default as this method was studied by Demler et al.
#'
#' @param group_count_init (_integer value_) the initial number of
#'  groups to form based on percentiles of `predicted_risk` values.
#'
#' @param group_count_min (_integer value_) the minimum number of
#'  groups to attempt running the GND test with. Only relevant if
#'  using `scalib_gnd_manual`.
#'
#' @param group_min_events_stop (_numeric value_) The lowest event count
#'   within a risk group that will not cause a hard stop (see details).
#'   Only relevant if using `scalib_gnd_manual`.
#'
#' @param group_min_events_warn (_numeric value_) The lowest event count
#'   within a risk group that will not cause a warning (see details).
#'   Only relevant if using `scalib_gnd_manual`.
#'
#' @param group_min_events (_numeric value_) The minimum number of events
#'   within a risk group (see details).
#'
#' @param group (_numeric vector_) Only relevant if using `scalib_gnd_manual`.
#'   An integer valued vector with group values starting at 1 and increasing
#'   by 1 for each additional group. These risk groups are used to run the
#'   GND test.
#'
#' @param verbose (_integer value_) If 0, no output will be printed.
#'  If 1, some output will be printed. If 2, _all_ output will be printed.
#'
#' @details `scalib_gnd` automatically forms risk groups, checks event counts
#'   in risk groups and, if necessary, collapses risk groups so that
#'   every group has an event count greater than a given threshold.
#'
#' `scalib_gnd_manual` completes the GND test, but requires the user to
#'   1. create a column in the scalib_object$data_inputs that contains
#'      risk group labels (see [predrisk_grp_prcnt])
#'   2. specify the column name of the risk group column and the
#'      predicted risk column.
#'
#' __Minimum event counts for risk groups__: Low event counts within any risk
#'  group may cause high variability in the GND test results. It is recommended
#'  that all risk groups have at least 5 events, and this is why the default
#'  value of `group_min_events_warn` is 5. If there are less than 2 events
#'  in any group, the GND test is unstable and risk groups should be collapsed.
#'  Therefore, the default value of `group_min_events_stop` is 2.
#'
#' @return an object of class `scalib` (see [scalib])
#'
#' @references
#'   Demler, O.V., Paynter, N.P. and Cook, N.R., 2015. Tests of calibration and
#'   goodness‐of‐fit in the survival setting. *Statistics in medicine*, 34(10),
#'   pp.1659-1680. DOI: 10.1002/sim.6428
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
#' # run GND test using 10 groups; apply some rules:
#' #  1. require at least 40 events per group
#' #  2. try to create 10 groups, and lump the lowest event frequency groups
#' #     if any groups have less than 40 events.
#' #  3. Hard stop if this is not obtainable with 5 or more groups.
#' #  4. set verbose = 2 to see every detail.
#' # (note that these rules are not a recommendation on how to use the test;
#' #  this example just shows the mechanics of the automatic group reduction.
#' #  The recommended values are the default values.)
#'
#' scalib_gnd(sc,
#'            group_min_events = 5,
#'            group_count_init = 10,
#'            group_count_min = 5,
#'            verbose = 2)

scalib_gnd <- function(scalib_object,
                       group_method = 'lump',
                       group_count_init = 10,
                       group_count_min = 2,
                       group_min_events = 5,
                       verbose = 0){


 check_call(
  match.call(),
  expected = list(
   'scalib_object' = list(
    class = 'scalib'
   ),
   'group_method' = list(
    type = 'character',
    options = c("lump", "redo")
   ),
   'group_count_init' = list(
    type = 'numeric',
    length = 1,
    lwr = 2,
    integer = TRUE
   ),
   'group_count_min' = list(
    type = 'numeric',
    length = 1,
    lwr = 2,
    integer = TRUE
   ),
   'verbose' = list(
    type = 'numeric',
    length = 1,
    lwr = 0,
    integer = TRUE
   )
  )
 )

 # Good ole' CRAN

 . = NULL
 event_time = NULL
 event_status = NULL
 value = NULL
 ._id_. = NULL
 count = NULL
 group = NULL
 N = NULL
 .update = NULL
 percent_expected = NULL
 events_expected = NULL
 group_n = NULL
 std.err = NULL
 surv = NULL
 gnd_component = NULL
 variance = NULL
 percent_observed = NULL
 events_observed = NULL
 gnd_pvalue = NULL
 gnd_chisq = NULL
 gnd_df = NULL
 gnd_data = NULL
 gnd_group_method = NULL

 check_scalib(scalib_object, pattern = '^gnd_',
              msg = paste("scalib_gnd() has already been applied",
                          "to this scalib_object.\nDo you want to",
                          "use scalib_gnd() on multiple predicted",
                          "values?\nIf so, try inputting them into",
                          "scalib() as a list, matrix, or data.frame"))

 pred_risk_cols <-  attr(scalib_object, 'pred_risk_cols')
 pred_horizon <- scalib_object$pred_horizon

 # browser()

 # observed event status and times
 # copy needed to avoid modifying input data
 dt <- copy(scalib_object$data_inputs)

 # curtailed events and times at prediction horizon
 dt <- dt[
  event_time > pred_horizon,
  `:=`(
   event_status = 0,
   event_time = pred_horizon
  )
 ]

 dt <- melt(dt,
            id.vars = c('event_status', 'event_time'),
            measure.vars = pred_risk_cols,
            variable.name = '._id_.')


 n_pred_uni <- dt[, .(count=length(unique(value))), by = ._id_.]

 if(any(n_pred_uni$count < group_count_init)){

  offenders <- n_pred_uni[count < group_count_init, ._id_., drop = TRUE]

  stop("The number of unique predicted risk values ",
       "should be > the number of groups.\nSee input column(s) ",
       paste_collapse(offenders, sep = ', ', last = 'and'),
       call. = FALSE)

 }


 # create rankings of variable values (used to make percentile groups)
 dt <- dt[, rank := frank(value), by = ._id_.]

 # create the initial groups
 dt <- dt[, group := ceiling((group_count_init)*rank/(.N+1)), by = ._id_.]

 if(verbose > 0)
  message("Checking event counts using ",
          group_count_init, " risk groups...",
          appendLF = FALSE)

 # count the number of events in each group for each ._id_.
 events <- dt[event_status == 1, .N, keyby = .(._id_., group)]

 group_count <- group_count_init

 repeat{

  # determine which ._id_. need to be updated with new groups
  # (this means >=1 of their the groups that have too few events)
  ids_to_update <-
   events[, .(.update = any(N < group_min_events)), by = ._id_.]

  ids_to_update <-
   ids_to_update[.update == TRUE, ._id_., drop = TRUE]

  if(is_empty(ids_to_update)){
   if(verbose > 0) message("okay", appendLF = TRUE)
   break
  }

  if(group_count < group_count_min){
   stop("There are too few events to run the GND test ",
        "using ", group_count_min, " groups.",
        call. = FALSE)
  }

  if(verbose > 0) message("too few; trying again", appendLF = TRUE)

  group_count <- group_count - 1

  if(verbose > 0)
   message("Checking event counts using ",
           group_count, " risk groups...",
           appendLF = FALSE)


  dt <- dt[
   ._id_. %in% ids_to_update,
   group := switch(
    group_method,
    'redo' = ceiling((group_count) * rank / (.N + 1)),
    'lump' = lump_group(group, events = events[._id_. == ._id_.])
   ),
   by = ._id_.
  ]

  events <- dt[event_status == 1, .N, keyby = .(._id_., group)]

 }

 dt_hoslem <- dt[
  , .(
   group_n = .N,
   # count the number of events in each group
   events_observed = sum(event_status),
   events_expected = sum(value)
  ),
  keyby = .(._id_., group)
 ]

 dt_hoslem <- dt_hoslem[, percent_expected := events_expected / group_n]

 dt_gnd <- dt[
  ,
  # summarizing the inputs by fitting a survfit
  # object to each group. Notably, we only need
  # two things (surv and std.err) from the survfit
  # object and they only need to be indexed at
  # their second to last value. Need to unclass()
  # survfit objects before pulling the data out.
  lapply(
   unclass(
    survival::survfit(
     formula = survival::Surv(event_time, event_status) ~ 1,
     se.fit = TRUE
    )
   )[c('surv', 'std.err')],
   function(x) x[length(x)-1]
  ),
  keyby = .(._id_., group)
 ]

 dt_gnd <- dt_gnd[, std.err := std.err * surv]

 dt_gnd <- dt_gnd[dt_hoslem]

 dt_gnd <- dt_gnd[
  # mutate; derive some columns
  , `:=`(
   percent_observed = 1 - surv,
   variance = std.err^2
  )
 ][ # mutate again, using the columns we just made
  , gnd_component := fifelse(
   variance == 0,
   yes = 0,
   no = (percent_observed - percent_expected)^2 / variance
  )
 ]

 dt_gnd <- dt_gnd[
  ,
  .(
   gnd_df = .N - 1,
   gnd_chisq = sum(gnd_component),
   gnd_data = list(
    data.table(
     group,
     group_n,
     events_observed,
     events_expected,
     percent_observed,
     percent_expected,
     variance,
     gnd_component)
   ),
   gnd_group_method = group_method
  ),
  by = ._id_.]

 dt_gnd <- dt_gnd[, gnd_pvalue := 1 - stats::pchisq(gnd_chisq, gnd_df)]

 dt_gnd <- dt_gnd[, .(._id_.,
                      gnd_df,
                      gnd_chisq,
                      gnd_pvalue,
                      gnd_data,
                      gnd_group_method) ]

 scalib_absorb(scalib_object, dt_gnd)

}



#' @rdname scalib_gnd
#' @export
scalib_gnd_manual <- function(scalib_object,
                              pred_risk_col,
                              group,
                              group_min_events_warn = 5,
                              group_min_events_stop = 2,
                              verbose = 0){

 check_call(
  match.call(),
  expected = list(
   'scalib_object' = list(
    class = 'scalib'
   ),
   'pred_risk_col' = list(
    type = 'character',
    length = 1
   ),
   'group' = list(
    type = 'numeric',
    length = nrow(scalib_object$data_inputs),
    integer = TRUE
   ),
   'group_min_events_warn' = list(
    type = 'numeric',
    length = 1,
    lwr = 5,
    integer = TRUE
   ),
   'group_min_events_stop' = list(
    type = 'numeric',
    length = 1,
    lwr = 2,
    integer = TRUE
   ),
   'verbose' = list(
    type = 'numeric',
    length = 1,
    lwr = 0,
    integer = TRUE
   )
  )
 )

 # good ole' CRAN...
 . = NULL
 event_time = NULL
 event_status = NULL
 ._pred_. = NULL
 percent_expected = NULL
 events_expected = NULL
 group_n = NULL
 std.err = NULL
 surv = NULL
 gnd_component = NULL
 variance = NULL
 percent_observed = NULL
 events_observed = NULL

 if(pred_risk_col %nin% names(scalib_object$data_inputs))
  stop(pred_risk_col, " is not a column in the survival ",
       "calibration object's input data", call. = FALSE)

 gnd_data <- copy(scalib_object$data_inputs)

 pred_horizon <- scalib_object$pred_horizon

 # curtailed events and times at prediction horizon
 gnd_data <- gnd_data[
  event_time > pred_horizon,
  `:=`(
   event_status = 0,
   event_time = pred_horizon
  )
 ]

 gnd_data$group <- group

 setnames(gnd_data, old = pred_risk_col, new = '._pred_.')

 hoslem_data <- gnd_data[
  , .(
   group_n = .N,
   # count the number of events in each group
   # to determine whether the GND test is safe
   events_observed = sum(event_status),
   events_expected = sum(._pred_.)
  ),
  keyby = group
 ][, percent_expected := events_expected / group_n]

 if (any(hoslem_data$events_observed < group_min_events_stop))
  stop(
   "at least 1 group contains < ", group_min_events_stop, " events.",
   call. = FALSE
  )

 if (any(hoslem_data$events_observed < group_min_events_warn)){

  msg <- paste0("at least 1 group contains < ",
                group_min_events_warn, " events")

  warning(msg, call. = FALSE)

 }

 gnd_data <- gnd_data[
  ,
  # summarizing the inputs by fitting a survfit
  # object to each group. Notably, we only need
  # two things (surv and std.err) from the survfit
  # object and they only need to be indexed at
  # their second to last value. Need to unclass()
  # survfit objects before pulling the data out.
  lapply(
   unclass(
    survival::survfit(
     formula = survival::Surv(event_time, event_status) ~ 1,
     se.fit = TRUE
    )
   )[c('surv', 'std.err')],
   function(x) x[length(x)-1]
  ),
  keyby = group
 ][, std.err := std.err * surv]

 gnd_data <- gnd_data[hoslem_data][
  # mutate; derive some columns
  , `:=`(
   percent_observed = 1 - surv,
   variance = std.err^2
  )
 ][ # mutate again, using the columns we just made
  , gnd_component := fifelse(
   variance == 0,
   yes = 0,
   no = (percent_observed - percent_expected)^2 / variance
  )
 ][ # select columns for output
  ,
  .(group,
    group_n,
    events_observed,
    events_expected,
    percent_observed,
    percent_expected,
    variance,
    gnd_component
  )
 ]

 if(verbose > 0)
  for(i in seq(nrow(gnd_data))){
   with(
    gnd_data[i, , drop = FALSE],
    message(
     "Group ", group, " (N = ", group_n, ")",
     "\n - ", events_observed, " events observed (",
     format(round(100*percent_observed, digits = 1), nsmall = 1),"%)",
     "\n - ", round(events_expected, digits = 0), " events expected (",
     format(round(100*percent_expected, digits = 1), nsmall = 1),"%)",
     #"\n - ", kmnrisk, " remain in risk pool at T = ", pred_horizon,
     "\n - GND component: ", round(gnd_component, digits = 3)
    )
   )
  }

 gnd_df <- nrow(gnd_data) - 1
 gnd_chisq <- sum(gnd_data$gnd_component)

 gnd_result <- data.table(
  ._id_. = pred_risk_col,
  gnd_df = gnd_df,
  gnd_chisq = gnd_chisq,
  gnd_pvalue = 1 - stats::pchisq(gnd_chisq, gnd_df),
  gnd_data = list(gnd_data),
  gnd_group_method = "custom"
 )

 scalib_absorb(scalib_object, gnd_result)

}


## This is no longer being used, but is being kept just in case
## I mess up the newer code and can't match the original GND function
## output anymore

# scalib_test_gnd_worker <- function(event_status,
#                                    event_time,
#                                    time_predict,
#                                    group_min_events_warn,
#                                    group_min_events_stop) {
#
#   if (sum(event_status) < group_min_events_stop)
#     return(c(est=0, stderr=0, num=0, stopped = -1))
#
#   stopped <- as.numeric(sum(event_status) < group_min_events_warn)
#
#   .surv <- survival::survfit(
#     formula = survival::Surv(event_time, event_status) ~ 1,
#     se.fit = TRUE
#   )
#
#   # initialize survival as 1; standard error as 0; number of events as 0
#   est <- 1; stderr <- 0; num <- 0
#
#   # if there is an event observed before admin censor time, then
#   # update survival estimate, standard error, and number of events
#   if(min(.surv$time) <= time_predict){
#
#     # find the last event time before censoring
#     i <- which.max(.surv$time[.surv$time <= time_predict])
#
#     # If this happens to be the last event in the entire dataset,
#     # the variance will be infinite! A workaround is to go back
#     # 1 time value at a time, until the std.err is not infinite
#     while(is.infinite(.surv$std.err[i])) i <- i-1
#
#     est    <- .surv$surv[i]
#     stderr <- .surv$std.err[i]
#
#   }
#
#   c(est     = est,
#     stderr  = stderr * est,
#     stopped = stopped)
#
# }
