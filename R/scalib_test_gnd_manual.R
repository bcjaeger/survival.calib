



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
#' @param event_time (_numeric vector_) observed event times
#'
#' @param time_predict (_numeric value_) the time of risk prediction.
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
#'   If 1, some details will be printed. If 2, all details will be printed.
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
#' @return an object of class 'survival_calib_test'
#'
#' @export
#'
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
#' risk_groups <- predrisk_grp_prcnt(risk_pred, g = 10)
#'
#' calib_test_gnd_manual(predicted_risk = risk_pred,
#'                 event_time = flchain$futime[-train_index],
#'                 event_status = flchain$death[-train_index],
#'                 time_predict = time_predict,
#'                 group = risk_groups,
#'                 verbose = 0)

scalib_test_gnd_manual <- function(scalib_object,
                                   pred_risk_col,
                                   group_min_events_warn = 5,
                                   group_min_events_stop = 2,
                                   verbose = 0,
                                   id_value = NULL){

  check_call(
    match.call(),
    expected = list(
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

  table_kapmeier <- scalib_object$data_inputs[
    ,
    as.list(
      gnd_worker(
        event_status = event_status,
        event_time = event_time,
        time_predict = scalib_object$pred_horizon,
        group_min_events_warn = group_min_events_warn,
        group_min_events_stop = group_min_events_stop
      )
    ),
    keyby = group
  ]

  if (any(table_kapmeier$stopped == -1))
    stop(
      "At least 1 group contains < ", group_min_events_stop, " events.",
      call. = FALSE
    )

  msg_warn <- "None"

  if (any(table_kapmeier$stopped == 1)){

    msg_warn <- paste0("At least 1 group contains < ",
                       group_min_events_warn, " events")

    if(verbose > 0) warning(msg_warn, call. = FALSE)

  }

  table_hoslem <- # summarize and sort by group
    scalib_object$data_inputs[
      , .(group_n = .N,
          events_observed = sum(event_status),
          percent_expected = sapply(.SD, mean),
          events_expected = sapply(.SD, sum)),
      .SDcols = pred_risk_col,
      keyby = group
    ][ # mutate
      , `:=`(
        percent_observed = 1 - table_kapmeier$est,
        variance = table_kapmeier$stderr^2
      )
    ][ # mutate again, using the columns we just made
      , GND_component := fifelse(
          variance == 0,
          yes = 0,
          no = (percent_observed - percent_expected)^2 / variance
      )
    ]

  if(verbose > 0)
    for(i in seq(nrow(table_hoslem))){
      with(
        table_hoslem[i, , drop = FALSE],
        message(
          "Group ", group, " (N = ", group_n, ")",
          "\n - ", events_observed, " events observed (",
          format(round(100*percent_observed, digits = 1), nsmall = 1),"%)",
          "\n - ", round(events_expected, digits = 0), " events expected (",
          format(round(100*percent_expected, digits = 1), nsmall = 1),"%)",
          #"\n - ", kmnrisk, " remain in risk pool at T = ", time_predict,
          "\n - GND component: ", round(GND_component, digits = 3)
        )
      )
    }

  gnd_df <- length(unique(scalib_object$data_inputs[, group])) - 1

  data.table(
    .pred_risk = pred_risk_col,
    GND_df = gnd_df,
    GND_chisq = sum(table_hoslem$GND_component),
    GND_pvalue = 1 - stats::pchisq(sum(table_hoslem$GND_component), gnd_df),
    GND_data = list(table_hoslem),
    GND_warning = msg_warn
  )

}

