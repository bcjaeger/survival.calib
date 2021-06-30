



#' Greenwood-Nam-D'Agostino (GND) Test
#'
#' Demler et al developed a modification of the Nam-D'Agostino test that has
#'   nominal type 1 error rates with censored outcomes. The GND test is also
#'   applicable in settings where the proportional hazards assumption is invalid.
#'
#' @param predicted_risk (_numeric vector_) predicted risk values for the
#'   event of interest at or before a given time.
#'
#' @param event_status (_numeric vector_) observed event status. The values
#'   of this vector should be 0 (event censored) and 1 (event observed).
#'
#' @param event_time (_numeric vector_) observed event times'
#'
#' @param time_predict (_numeric value_) the time that risk predictions are
#'   computed for. For example, to assess calibration of 10-year predicted
#'   risk, set `time_predict = 10`.
#'
#' @param group (_numeric vector_) risk group assignment. The GND chi-square
#'   test statistic is based on the difference between predicted and observed
#'   event counts in each risk group.
#'
#' @param group_min_events_warn (_numeric value_) The lowest event count
#'   within a risk group that will not cause a warning (see details).
#'
#' @param group_min_events_stop (_numeric value_) The lowest event count
#'   within a risk group that will not cause a hard stop (see details).
#'
#' @param verbose (_integer value_) If 0, no output will be printed.
#'   If 1, details about events in each group will be printed.
#'
#' @details
#'
#' __Minimum event counts for risk groups__: Low event counts within any risk
#'  group may cause high variability in the GND test results. It is recommended
#'  that all risk groups have at least 5 events, and this is why the default
#'  value of `group_min_events_warn` is 5. If there are less than 2 events
#'  in any group, the GND test is unstable and risk groups should be collapsed.
#'  Therefore, the default value of `group_min_events_stop` is 2.
#'
#' @references
#'   Demler, O.V., Paynter, N.P. and Cook, N.R., 2015. Tests of calibration and
#'   goodness‐of‐fit in the survival setting. *Statistics in medicine*, 34(10),
#'   pp.1659-1680. DOI: 10.1002/sim.6428
#'
#' @return an object of class 'survival.calib_gnd_test'
#'
#' @export
#'
#' @examples
#'
#' library(survival)
#' library(riskRegression)
#' train_index <- 1:3000
#'
#' risk_mdl <- coxph(data = flchain[train_index, ],
#'                   x = TRUE,
#'                   formula = Surv(futime, death)~age+sex+flc.grp+lambda)
#'
#' time_predict <- 4000
#'
#' risk_pred <- predictRisk(risk_mdl,
#'                          newdata = flchain[-train_index, ],
#'                          times = time_predict)
#'
#' #split into deciles
#' risk_groups <- cut_percentiles(risk_pred, g = 10)
#'
#' gnd_test_manual(predicted_risk = risk_pred,
#'                 event_time = flchain$futime[-train_index],
#'                 event_status = flchain$death[-train_index],
#'                 time_predict = time_predict,
#'                 group = risk_groups,
#'                 verbose = 0)

gnd_test_manual = function(predicted_risk,
                           event_status,
                           event_time,
                           time_predict,
                           group,
                           group_min_events_warn = 5,
                           group_min_events_stop = 2,
                           verbose = 0){


  check_call(
    match.call(),
    expected = list(
      'predicted_risk' = list(type = 'numeric', lwr = 0, upr = 1),
      'event_status' = list(type = 'numeric', uni = c(0, 1)),
      'event_time' = list(type = 'numeric', lwr = 0),
      'time_predict' = list(type = 'numeric', lwr = 0, length = 1),
      'group' = list(type = 'numeric', lwr = 0),
      'group_min_events_warn' = list(type = 'numeric', length = 1, lwr = 5),
      'group_min_events_stop' = list(type = 'numeric', length = 1, lwr = 2),
      'verbose' = list(type = 'numeric', length = 1, lwr = 0)
    )
  )




  data_curtailed = data.frame(
    # predicted risk of event
    predicted_risk = predicted_risk,
    # observed event times, curtailed at time of censor
    event_time = ifelse(test = event_time > time_predict,
                        yes = time_predict,
                        no = event_time),
    # observed event status, curtailed at time of censor
    event_status = ifelse(test = event_time > time_predict,
                          yes = 0,
                          no = event_status),
    # used to compute the group size
    count = 1,
    # grouping assignments based on risk predictions
    group = group
  )

  table_kapmeier <- t(
    vapply(
      X = split(data_curtailed, f = data_curtailed$group),
      FUN = gnd_calib_worker,
      time_predict = time_predict,
      group_min_events_warn = group_min_events_warn,
      group_min_events_stop = group_min_events_stop,
      FUN.VALUE = rep(0, 4)
    )
  )

  if (any(table_kapmeier[, 'stopped'] == -1))
    stop(
      "At least 1 group contains < ", group_min_events_stop, " events.",
      call. = FALSE
    )

  msg_warn <- "None"

  if (any(table_kapmeier[, 'stopped'] == 1)){

    msg_warn <- paste0("At least 1 group contains < ",
                       group_min_events_warn, " events")

    if(verbose > 0) warning(msg_warn, call. = FALSE)

  }

  # TODO Add this to details of documentation
  # , and this may cause high variability in the GND test results
  # (see Demler, Paynter, Cook 'Tests of Calibration and Goodness of Fit
  #   in the Survival Setting, DOI: 10.1002/sim.6428).",

  table_hoslem <- with(
    data_curtailed,
    data.frame(
      group_label      = sort(unique(group)),
      group_n          = tapply(count,          group, sum),
      events_observed  = tapply(event_status,   group, sum),
      events_expected  = tapply(predicted_risk, group, sum),
      percent_expected = tapply(predicted_risk, group, mean)
    )
  )


  table_hoslem$percent_observed  = 1 - table_kapmeier[, 'est']
  table_hoslem$variance   = table_kapmeier[, 'stderr']^2

  #table_hoslem$kmnrisk = table_kapmeier[, 'num']
  #table_hoslem$kmnum   = table_hoslem$kmperc * table_hoslem$totaln

  table_hoslem$GND_component <- with(
    table_hoslem,
    expr = ifelse(
      test = variance == 0,
      yes = 0,
      no = (percent_observed - percent_expected)^2 / variance
    )
  )

  if(verbose > 0)
    for(i in seq(nrow(table_hoslem))){
      with(
        table_hoslem[i, , drop = FALSE],
        message(
          "Group ", group_label, " (N = ", group_n, ")",
          "\n - ", events_observed, " events observed (",
          format(round(100*percent_observed, digits = 1), nsmall = 1),"%)",
          "\n - ", round(events_expected, digits = 0), " events expected (",
          format(round(100*percent_expected, digits = 1), nsmall = 1),"%)",
          #"\n - ", kmnrisk, " remain in risk pool at T = ", time_predict,
          "\n - GND component: ", round(GND_component, digits = 3)
        )
      )
    }

  gnd_df <- length(unique(data_curtailed$group)) - 1

  gnd_test <- data.frame(
    GND_df = gnd_df,
    GND_chisq = sum(table_hoslem$GND_component),
    GND_pvalue = 1 - stats::pchisq(sum(table_hoslem$GND_component), gnd_df),
    row.names = NULL
  )

  structure(
    .Data = list(statistic = gnd_test,
                 data = table_hoslem,
                 warnings = msg_warn),
    class = "survival.calib_gnd_test"
  )


}



