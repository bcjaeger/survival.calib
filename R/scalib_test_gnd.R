

#' Greenwood-Nam-D'Agostino (GND) Test on Autopilot
#'
#' The GND test is based on groupwise error rates for observed versus
#'   expected risk, and the test is unstable when any group contains
#'   < 5 events (see Demler et al.). The purpose of `calib_test_gnd` is
#'   to automatically check event counts in risk groups and, if
#'   necessary, collapse risk groups so that every group has an
#'   event count greater than a given threshold.
#'
#' @inheritParams calib_test_gnd_manual
#'
#' @param group_method (_character value_; 'lump' or 'redo') If 'lump', then
#'   a 'lumping' procedure will be applied whenever a group has less than
#'   `group_min_events_warn` events. The lumping procedure will identify
#'   whichever group has the lowest event count and assign members of that
#'   group to the group with too few events. If 'redo', then the groups
#'   will be re-done using `cut_percentile` but with one less group.
#'   'lump' is the default as this method was studied by Demler et al.
#'
#' @param group_count_init (_integer value_) the initial number of
#'  groups to form based on percentiles of `predicted_risk` values.
#'
#' @param group_count_min (_integer value_) the minimum number of
#'  groups to attempt running the GND test with. If
#'
#' @param verbose (_integer value_) If 0, no output will be printed.
#'  If 1, then output from group reduction will be printed.
#'  If 2, then output from group reduction __and__ [calib_test_gnd_manual]
#'  will be printed.
#'
#' @return an object of class 'survival_calib_test'
#'
#' @references
#'   Demler, O.V., Paynter, N.P. and Cook, N.R., 2015. Tests of calibration and
#'   goodness‐of‐fit in the survival setting. *Statistics in medicine*, 34(10),
#'   pp.1659-1680. DOI: 10.1002/sim.6428
#'
#' @export
#'
#' @examples
#' # packages
#' library(riskRegression)
#' library(survival)
#'
#' # set index for training data
#' train_index <- 1:3000
#'
#' # fit cox PH model using train data
#' risk_mdl <- coxph(data = flchain[train_index, ],
#'                   x = TRUE,
#'                   formula = Surv(futime, death)~age+sex+flc.grp+lambda)
#'
#' # set time of prediction
#' time_predict <- 4000
#'
#' # predict risk in the testing data
#' risk_pred <- predictRisk(risk_mdl,
#'                          newdata = flchain[-train_index, ],
#'                          times = time_predict)
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
#' calib_test_gnd(predicted_risk = risk_pred,
#'               event_time = flchain$futime[-train_index],
#'               event_status = flchain$death[-train_index],
#'               time_predict = time_predict,
#'               group_min_events_warn = 40,
#'               group_count_init = 10,
#'               group_count_min = 5,
#'               verbose = 2)

scalib_test_gnd <- function(scalib_object,
                            group_method = 'lump',
                            group_count_init = 10,
                            group_count_min = 2,
                            group_min_events_warn = 5,
                            group_min_events_stop = 2,
                            verbose = 0){

  check_call(
    match.call(),
    expected = list(
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

  input_names <- names(scalib_object$data_inputs)
  pred_risk_indices <- grep(pattern = '^pred_risk', x = input_names)
  pred_risk_cols <- input_names[pred_risk_indices]

  output <- vector(mode = 'list', length = length(pred_risk_cols))

  for(i in seq_along(pred_risk_cols)){

    output[[i]] <- scalib_test_gnd_worker(
      scalib_object = scalib_object,
      pred_risk_col = pred_risk_cols[i],
      group_method = group_method,
      group_count_init = group_count_init,
      group_count_min = group_count_min,
      group_min_events_warn = group_min_events_warn,
      group_min_events_stop = group_min_events_stop,
      verbose = verbose
    )

  }

  if (is_empty(scalib_object$data_outputs)) {

    scalib_object$data_outputs <-
      cbind(scalib_object$data_outputs,
            rbindlist(output))

  } else {

    scalib_object$data_outputs <-
      merge(x = scalib_object$data_outputs,
            y = rbindlist(output),
            all.x = TRUE,
            by = '.pred_risk')

  }

  scalib_object

}


scalib_test_gnd_worker <- function(scalib_object,
                                   pred_risk_col,
                                   group_method,
                                   group_count_init,
                                   group_count_min,
                                   group_min_events_warn,
                                   group_min_events_stop,
                                   verbose){

  scalib_object$data_inputs$group <-
    predrisk_grp_gnd(
      x = scalib_object$data_inputs[[pred_risk_col]],
      event_status = scalib_object$data_inputs$event_status,
      group_method = group_method,
      group_count_init = group_count_init,
      group_count_min = group_count_min,
      group_min_events_warn = group_min_events_warn,
      group_min_events_stop = group_min_events_stop,
      verbose = verbose
    )

  GND_result <-
    scalib_test_gnd_manual(
      scalib_object = scalib_object,
      pred_risk_col = pred_risk_col,
      group_min_events_warn = group_min_events_warn,
      group_min_events_stop = group_min_events_stop,
      verbose = max(0, verbose - 1)
    )

  if(verbose == 1) message("..Done")

  GND_result

}

predrisk_grp_gnd <- function(x,
                             event_status,
                             group_method,
                             group_count_init,
                             group_count_min,
                             group_min_events_warn,
                             group_min_events_stop,
                             verbose){

  too_few_groups <- FALSE
  group_count <- group_count_init

  repeat {

    if(verbose > 0)
      message("Checking event counts using ",
              group_count, " risk groups...",
              appendLF = FALSE)

    if(group_count == group_count_init){
      group = predrisk_grp_prcnt(
        x = x,
        g = group_count
      )
    }

    if(group_count < group_count_init){

      group = switch(
        group_method,
        "redo" = predrisk_grp_prcnt(
          x = scalib_object$data_inputs[[pred_risk_col]],
          g = group_count
        ),
        "lump" = lump_group(
          group = group,
          events = group_event_counts,
          min_size = group_min_events_warn
        )
      )


    }

    group_event_table <-
      table(group = group,
            event_status = event_status)

    group_event_counts <-
      subset(
        as.data.frame(group_event_table, stringsAsFactors = FALSE),
        event_status == 1
      )

    # Seems silly, but this is needed to keep the groups from getting
    # character sorted; i.e., 1, 10, 11, ... 19, 2, 20, etc.
    group_event_counts$group <- as.numeric(group_event_counts$group)

    if(all(group_event_counts$Freq > group_min_events_warn)){
      if(verbose > 0) message("okay")
      break
    }

    group_count <- group_count - 1
    if(verbose > 0) message("too few; trying again")

    if(group_count < group_count_min){

      stop("There are too few events to run the GND test ",
           "using ", group_count_min, " groups.",
           call. = FALSE)

    }

  }

  if(verbose > 0)
    message("Attempting GND test with ", group_count, " risk groups.",
            appendLF = verbose > 1)

  group

}
