

#' Calibration Slope using Hazard Regression (HARE)
#'
#' @inheritParams calib_test_gnd_manual
#'
#' @return an object of class 'survival_calib_slope'
#'
#' @export
#'
#' @examples
#'
#' # packages
#' library(riskRegression)
#' library(survival)
#' library(ggplot2)
#'
#' # set index for training data
#' set.seed(329)
#'
#' model_data <- na.omit(flchain)
#'
#' train_index <- sample(nrow(model_data), size = 1000)
#'
#' # fit cox PH model using train data
#' risk_mdl <- coxph(
#'   data = model_data[train_index, ],
#'   x = TRUE,
#'   formula = Surv(futime, death) ~ .
#' )
#'
#' # set time of prediction
#' time_predict <- 1500
#'
#' # predict risk in the testing data
#' risk_pred <- predictRisk(risk_mdl,
#'                          newdata = model_data[-train_index, ],
#'                          times = time_predict)
#'
#' calslope_hare <- calib_slope_hare(
#'   predicted_risk = risk_pred,
#'   event_status = model_data$death[-train_index],
#'   event_time = model_data$futime[-train_index],
#'   time_predict = time_predict,
#'   verbose = 2
#' )
#'
#' calslope_hare
#'
#' ggplot(calslope_hare$slope) +
#'   aes(x = predicted, y = observed) +
#'   geom_line() +
#'   geom_abline(col = 'red', linetype = 2) +
#'   geom_hline(yintercept = 0, col = 'grey') +
#'   theme_bw() +
#'   theme(panel.grid = element_blank()) +
#'   coord_cartesian(xlim = c(0, 1),
#'                   ylim = c(-0.1, 1)) +
#'   geom_segment(data = calslope_hare$bins,
#'                size = 2,
#'                mapping = aes(x = x,
#'                              y = y,
#'                              xend = xend,
#'                              yend = yend,
#'                              color = factor(event_status)))

calib_slope_hare <- function(predicted_risk,
                             event_status,
                             event_time,
                             time_predict,
                             verbose = 0){

  UseMethod("calib_slope_hare")

}

#' @export
calib_slope_hare.default <- function(predicted_risk,
                                     event_status,
                                     event_time,
                                     time_predict,
                                     verbose = 0){

  .class <- class(predicted_risk)[1]

  stop("No applicable method for 'calib_slope_hare' that can be",
       "\napplied to an object of class ", .class, call. = FALSE)
}

#' @export
calib_slope_hare.list <- function(predicted_risk,
                                  event_status,
                                  event_time,
                                  time_predict,
                                  verbose = 0){

  predrisk_items_are_numeric <- sapply(predicted_risk, is.numeric)

  if(!all(predrisk_items_are_numeric))
    stop("calib_slope_hare only handles lists if each",
         " item in the list is numeric",
         call. = FALSE)

  list_names <- names(predicted_risk)
  list_length <- length(predicted_risk)

  if(list_length == 0)
    return(NULL)

  if(is.null(list_names))
    list_names <- seq(list_length)

  # don't use lapply; looping over two things
  # don't use purrr; too much dependence

  list_output <- vector(mode = 'list', length = list_length)

  for(i in seq_along(list_output)){
    list_output[[i]] <-
      calib_slope_hare.numeric(
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

calib_slope_hare.matrix <- function(predicted_risk,
                                    event_status,
                                    event_time,
                                    time_predict,
                                    verbose = 0){

  .f <- ifelse(ncol(predicted_risk) == 1,
               yes = as.numeric,
               no = as_list_by_column)

  calib_slope_hare(
    predicted_risk = .f(predicted_risk),
    event_status = event_status,
    event_time = event_time,
    time_predict = time_predict,
    verbose = verbose
  )

}

#' @export
calib_slope_hare.numeric <- function(predicted_risk,
                                     event_status,
                                     event_time,
                                     time_predict,
                                     verbose = 0,
                                     id_value = NULL){

  check_call(
    match.call(),
    expected = list(
      'predicted_risk' = list(type = 'numeric', lwr = 0, upr = 1),
      'event_status' = list(type = 'numeric', uni = c(0, 1)),
      'event_time' = list(type = 'numeric', lwr = 0),
      'time_predict' = list(type = 'numeric', lwr = 0),
      'verbose' = list(type = 'numeric', length = 1, lwr = 0)
    )
  )

  predicted_risk_lnlc <- matrix(log_neg_log_complement(predicted_risk),
                                ncol = 1)

  if(verbose >= 1)
    message("Fitting hazard regression model...", appendLF = verbose >= 2)

  hare_fit <- polspline::hare(data = event_time,
                              delta = event_status,
                              cov = predicted_risk_lnlc,
                              silent = verbose < 2)

  if(verbose >= 1) message("Done.")

  observed_risk <-
    polspline::phare(q = time_predict,
                     cov = predicted_risk_lnlc,
                     fit = hare_fit)

  .abs_error <- abs(observed_risk - predicted_risk)

  data_summary <- tibble::tibble(
    abs_error = list(.abs_error),
    ici = mean(.abs_error),
    e50 = stats::quantile(.abs_error, 1/2),
    e90 = stats::quantile(.abs_error, 9/10),
    emax = max(.abs_error)
  )

  predicted_risk_grid <-
    seq(stats::quantile(predicted_risk, probs = 0.01),
        stats::quantile(predicted_risk, probs = 0.99),
        length.out = 100)

  # probability of having an event by time_predict,
  # conditional on having a predicted risk of x,
  # where x is a value on the predicted risk grid
  observed_risk_grid <-
    polspline::phare(q = time_predict,
                     cov = log_neg_log_complement(predicted_risk_grid),
                     fit = hare_fit)

  data_calib <- tibble::tibble(predicted = predicted_risk_grid,
                               observed = observed_risk_grid)

  # if there is no id value, don't make it part of the output
  if(is.null(id_value)){
    return(
      survival_calib_slope_init(
        time_predict = time_predict,
        data = data_calib,
        summary = data_summary
      )
    )
  }

  # if there is an id value, make it the first column of the output
  survival_calib_slope_init(
    time_predict = time_predict,
    data = tibble::add_column(.data = data_calib,
                              id = id_value,
                              .before = 1),
    summary = tibble::add_column(.data = data_summary,
                                 id = id_value,
                                 .before = 1)
  )

}


survival_calib_slope_init <- function(time_predict, fit, data, summary){

  structure(
    .Data = list(
      time_predict = time_predict,
      data = data,
      summary = summary
    ),
    class = "survival_calib_slope"
  )

}

#' Coerce to a Tibble
#'
#' @param x an object of class `survival_calib_slope`
#' @param ... not used
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @export
#'
#' @examples
#'
as_tibble.survival_calib_slope <- function(x, ...){

  if(is.null(x$data$id))
    cbind(x$data, time_predict = x$time_predict, x$summary)

  split_smry <- split(x$summary, f = x$summary$id)

  split_data <- split(x$data, f = x$data$id)

  output <- vector(mode = 'list', length = length(split_smry))

  for(i in seq_along(output))
    output[[i]] <- cbind(split_data[[i]],
                         time_predict = x$time_predict,
                         split_smry[[i]][, -1L]) # dropping id

  Reduce(rbind, output)

}
