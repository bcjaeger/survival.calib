



#' Modified Greenwood-Nam D'Agostino Test
#'
#' @param predicted_risk
#' @param event_time
#' @param event_status
#' @param group
#' @param time_admin_censor
#' @param group_min_events_warn
#' @param group_min_events_stop
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
gnd_test_manual = function(predicted_risk,
                           event_time,
                           event_status,
                           group,
                           time_admin_censor,
                           group_min_events_warn = 5,
                           group_min_events_stop = 2,
                           verbose = 0){

  data_curtailed = data.frame(
    # predicted risk of event
    predicted_risk = predicted_risk,
    # observed event times, curtailed at time of censor
    event_time = ifelse(test = event_time > time_admin_censor,
                        yes = time_admin_censor,
                        no = event_time),
    # observed event status, curtailed at time of censor
    event_status = ifelse(test = event_time > time_admin_censor,
                          yes = 0,
                          no = event_status),
    # used to compute the group size
    count = 1,
    # grouping assignments based on risk predictions
    group = group
  )

  km_tab <- t(
    vapply(
      X = split(data_curtailed, f = data_curtailed$group),
      FUN = gnd_calib_worker,
      time_admin_censor = time_admin_censor,
      group_min_events_warn = group_min_events_warn,
      group_min_events_stop = group_min_events_stop,
      FUN.VALUE = rep(0, 4)
    )
  )

  if (any(km_tab[, 'stopped'] == -1))
    stop(
      "At least 1 group contains < ", group_min_events_stop, " events.",
      call. = FALSE
    )

  if (any(km_tab[, 'stopped'] == 1)) warning(
    "At least 1 group contains < ", group_min_events_warn, " events",
    call. = FALSE
  )

  # TODO Add this to details of documentation
  # , and this may cause high variability in the GND test results
  # (see Demler, Paynter, Cook 'Tests of Calibration and Goodness of Fit
  #   in the Survival Setting, DOI: 10.1002/sim.6428).",

  hl_tab <- with(
    data_curtailed,
    data.frame(
      group_label      = sort(unique(group)),
      group_n          = tapply(count,          group, sum),
      events_observed  = tapply(event_status,   group, sum),
      events_expected  = tapply(predicted_risk, group, sum),
      percent_expected = tapply(predicted_risk, group, mean)
    )
  )


  hl_tab$percent_observed  = 1 - km_tab[, 'est']
  hl_tab$variance   = km_tab[, 'stderr']^2

  #hl_tab$kmnrisk = km_tab[, 'num']
  #hl_tab$kmnum   = hl_tab$kmperc * hl_tab$totaln

  hl_tab$GND_component <- with(
    hl_tab,
    expr = ifelse(
      test = variance == 0,
      yes = 0,
      no = (percent_observed - percent_expected)^2 / variance
    )
  )

  if(verbose > 0)
    for(i in seq(nrow(hl_tab))){
      with(
        hl_tab[i, , drop = FALSE],
        message(
          "Group ", group_label, " (N = ", group_n, ")",
          "\n - ", events_observed, " events observed (",
          format(round(100*percent_observed, digits = 1), nsmall = 1),"%)",
          "\n - ", round(events_expected, digits = 0), " events expected (",
          format(round(100*percent_expected, digits = 1), nsmall = 1),"%)",
          #"\n - ", kmnrisk, " remain in risk pool at T = ", time_admin_censor,
          "\n - GND component: ", round(GND_component, digits = 3)
        )
      )
    }

  gnd_df <- length(unique(data_curtailed$group)) - 1

  hl_tab

  data.frame(
    GND_df = gnd_df,
    GND_chisq = sum(hl_tab$GND_component),
    GND_pvalue = 1 - stats::pchisq(sum(hl_tab$GND_component), gnd_df),
    row.names = NULL
  )

}

