

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

calib_test_gnd <- function(predicted_risk,
                           event_time,
                           event_status,
                           time_predict,
                           group_method = 'lump',
                           group_count_init = 10,
                           group_count_min = 2,
                           group_min_events_warn = 5,
                           group_min_events_stop = 2,
                           verbose = 0){

  UseMethod("calib_test_gnd")

}

#' @export
calib_test_gnd.default <- function(predicted_risk,
                                   event_time,
                                   event_status,
                                   time_predict,
                                   group_method = 'lump',
                                   group_count_init = 10,
                                   group_count_min = 2,
                                   group_min_events_warn = 5,
                                   group_min_events_stop = 2,
                                   verbose = 0){

  .class <- class(predicted_risk)[1]

  stop("No applicable method for 'calib_test_gnd' that can be",
       "\napplied to an object of class ", .class, call. = FALSE)

}

#' @export
calib_test_gnd.list <- function(predicted_risk,
                                event_time,
                                event_status,
                                time_predict,
                                group_method = 'lump',
                                group_count_init = 10,
                                group_count_min = 2,
                                group_min_events_warn = 5,
                                group_min_events_stop = 2,
                                verbose = 0){

  predrisk_items_are_numeric <- sapply(predicted_risk, is.numeric)

  if(!all(predrisk_items_are_numeric))
    stop("calib_test_gnd only handles lists if each",
         " item in the list is numeric", call. = FALSE)

  list_names <- names(predicted_risk)
  list_length <- length(predicted_risk)

  if(list_length == 0)
    return(NULL)

  if(is.null(list_names))
    list_names <- seq(list_length)

  list_output <- vector(mode = 'list', length = list_length)

  # don't use lapply; looping over two things
  # don't use purrr; too much dependence

  for(i in seq_along(list_output)){
    list_output[[i]] <-
      calib_test_gnd.numeric(
        predicted_risk = predicted_risk[[i]],
        event_status = event_status,
        event_time = event_time,
        time_predict = time_predict,
        verbose = verbose,
        id_value = list_names[i]
      )
  }

  calib_bind(list_output)

}


#' @export
calib_test_gnd.matrix <- function(predicted_risk,
                                  event_time,
                                  event_status,
                                  time_predict,
                                  group_method = 'lump',
                                  group_count_init = 10,
                                  group_count_min = 2,
                                  group_min_events_warn = 5,
                                  group_min_events_stop = 2,
                                  verbose = 0){

  .f <- ifelse(ncol(predicted_risk) == 1,
               yes = as.numeric,
               no = as_list_by_column)

  calib_test_gnd(
    predicted_risk = .f(predicted_risk),
    event_time = event_time,
    event_status = event_status,
    time_predict = time_predict,
    group_method = group_method,
    group_count_init = group_count_init,
    group_count_min = group_count_min,
    group_min_events_warn = group_min_events_warn,
    group_min_events_stop = group_min_events_stop,
    verbose = verbose
  )

}

#' @export
calib_test_gnd.numeric <- function(predicted_risk,
                                   event_time,
                                   event_status,
                                   time_predict,
                                   group_method = 'lump',
                                   group_count_init = 10,
                                   group_count_min = 2,
                                   group_min_events_warn = 5,
                                   group_min_events_stop = 2,
                                   verbose = 0,
                                   id_value = NULL) {

  check_call(
    match.call(),
    expected = list(
      'group_method' = list(type = 'character', options = c("lump", "redo")),
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

  status_before_horizon <- event_status
  status_before_horizon[event_time > time_predict] <- 0

  too_few_groups <- FALSE
  group_count <- group_count_init

  repeat {

    if(verbose > 0)
      message("Checking event counts using ", group_count, " risk groups...",
              appendLF = FALSE)

    if(group_count == group_count_init){
      group = predrisk_grp_prcnt(x = predicted_risk, g = group_count)
    }

    if(group_count < group_count_init){

      group = switch(
        group_method,
        "redo" = predrisk_grp_prcnt(predicted_risk, g = group_count),
        "lump" = lump_group(group = group,
                            events = group_event_counts,
                            min_size = group_min_events_warn)
      )


    }

    group_event_table <-
      table(group = group, event_status = status_before_horizon)

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
      too_few_groups <- TRUE
      break
    }

  }

  if(too_few_groups){

    stop("There are too few events to run the GND test ",
         "using ", group_count_min, " groups.",
         call. = FALSE)

  } else {

    if(verbose > 0)
      message("Attempting GND test with ", group_count, " risk groups.",
              appendLF = verbose > 1)

    GND_result <- calib_test_gnd_manual(
      predicted_risk = predicted_risk,
      event_time = event_time,
      event_status = event_status,
      group = group,
      time_predict = time_predict,
      group_min_events_warn = group_min_events_warn,
      group_min_events_stop = group_min_events_stop,
      verbose = max(0, verbose - 1),
      id_value = id_value
    )

    if(verbose == 1) message("..Done")

  }


  GND_result

}

