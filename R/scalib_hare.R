
#' Calibration Slope using Hazard Regression (HARE)
#'
#' @description TODO: add description
#'
#' @inheritParams scalib_gnd
#'
#' @param hare_penalty the parameter to be used in the AIC criterion. The method chooses the number of knots that minimizes `-2 * loglikelihood + penalty * (dimension)`. The default is to use penalty = log(samplesize) as in BIC. The effect of this parameter is summarized in [summary.hare.][polspline::summary.hare]
#'
#' @param hare_max_dimension maximum dimension (default is `6 * length(data)^0.2.`
#'
#' @param hare_prophaz should the model selection be restricted to proportional hazards models?
#'
#' @param hare_additive should the model selection be restricted to additive models?
#'
#' @param hare_linear_time (_logical value_) should the effect of time be linear?
#'
#' @param hare_linear_risk (_logical value_) should the effect of risk be linear?
#'
#' @param hare_fit [hare][polspline::hare] object. If `fit` is specified, [hare][polspline::hare] adds basis functions starting with those in fit.
#'
#' @return an object of class [scalib]
#'
#' @export
#'
#' @examples
#'
#' # packages
#' library(ggplot2)
#' library(tidyr)
#' library(dplyr)
#'
#' sc <- scalib(pred_risk = pbc_scalib$predrisk,
#'              pred_horizon = 2500,
#'              event_time = pbc_scalib$test$time,
#'              event_status = pbc_scalib$test$status)
#'
#' sc_hare <- scalib_hare(sc)
#'
#' print(sc_hare)
#'
#' data_gg <- sc_hare$data_outputs |>
#'   select(._id_., hare_data_plot) |>
#'   unnest(hare_data_plot)
#'
#' data_bins <- predrisk_bin_segments(sc_hare, bin_count = 30) |>
#'  mutate(._id_.)
#'
#' id_labels <- c(prop_hazard = "Cox proportional hazards",
#'                rsf_axis = "Random survival forest (axis based splitting)",
#'                gradient_booster = "Gradient boosted decision trees",
#'                rsf_oblique = "Random survival forest (oblique splitting)")
#'
#' ggplot(data_gg) +
#'   aes(x = predicted, y = observed) +
#'   geom_line() +
#'   geom_abline(col = 'grey', linetype = 2) +
#'   geom_hline(yintercept = 0, col = 'grey') +
#'   theme_bw() +
#'   theme(panel.grid = element_blank()) +
#'   coord_cartesian(xlim = c(0, 1),
#'                   ylim = c(-0.1, 1)) +
#'   geom_segment(data = data_bins,
#'                size = 4,
#'                alpha = 0.50,
#'                col = 'grey',
#'                mapping = aes(x = x,
#'                              y = y,
#'                              xend = xend,
#'                              yend = yend)) +
#'  facet_wrap(~._id_., labeller = labeller(._id_. = id_labels)) +
#'  labs(x = 'Predicted risk', y = 'Observed risk')


scalib_hare <- function(scalib_object,
                        verbose = 0,
                        hare_penalty = NULL,
                        hare_max_dimension = NULL,
                        hare_prophaz = FALSE,
                        hare_additive = FALSE,
                        hare_linear_risk = FALSE,
                        hare_linear_time = FALSE,
                        hare_fit = NULL){


  check_call(
    match.call(),
    expected = list(
      'scalib_object' = list(
        class = 'scalib'
      ),
      'verbose' = list(
        type = 'numeric',
        length = 1,
        lwr = 0,
        integer = TRUE
      ),
      'hare_penalty' = list(
        type = 'numeric',
        length = 1,
        lwr = 0
      ),
      'hare_max_dimension' = list(
        type = 'numeric',
        length = 1,
        lwr = 0
      ),
      'hare_prophaz' = list(
        type = 'logical',
        length = 1
      ),
      'hare_additive' = list(
        type = 'logical',
        length = 1
      ),
      'hare_linear_time' = list(
        type = 'logical',
        length = 1
      ),
      'hare_linear_risk' = list(
        type = 'logical',
        length = 1
      ),
      'hare_fit' = list(
        class = 'hare'
      )
    )
  )

  check_scalib(scalib_object, pattern = '^hare_',
               msg = paste("scalib_hare() has already been applied",
                           "to this scalib_object.\nDo you want to",
                           "use scalib_hare() on multiple predicted",
                           "values?\nIf so, try inputting them into",
                           "scalib() as a list, matrix, or data.frame"))

  if (!requireNamespace("polspline", quietly = TRUE)) {
    stop("Package `polspline` is not available,",
         " but needed for `scalib_hare()`.",
         call. = FALSE)
  }

  pred_risk_cols <- attr(scalib_object, 'pred_risk_cols')

  hare_args <- list(prophaz = hare_prophaz,
                    additive = hare_additive,
                    silent = verbose < 2)

  if(!is.null(hare_penalty))       hare_args$penalty <- hare_penalty
  if(!is.null(hare_max_dimension)) hare_args$maxdim  <- hare_max_dimension
  if(!is.null(hare_fit))           hare_args$fit     <- hare_fit

  hare_linear <- c()

  if(isTRUE(hare_linear_time)) hare_linear <- c(0)
  if(isTRUE(hare_linear_risk)) hare_linear <- c(hare_linear, 1)
  if(!is_empty(hare_linear)) hare_args$linear <- hare_linear

  result <- vector(mode = 'list', length = length(pred_risk_cols))

  for(i in seq_along(pred_risk_cols)){

    ._pred_. <- getElement(scalib_object$data_inputs, pred_risk_cols[i])
    ._pred_lnlc_. <- log_neg_log_complement(._pred_.)

    hare_args$data  <- scalib_object$data_inputs$event_time
    hare_args$delta <- scalib_object$data_inputs$event_status
    hare_args$cov   <- ._pred_lnlc_.

    if(verbose >= 1)
      message("Fitting hazard regression model...", appendLF = verbose >= 2)

    hare_fit <- do.call(polspline::hare, args = hare_args)

    if(verbose >= 1) message("Done.")

    ._obs_. <-
      do.call(
        polspline::phare,
        args = list(
          q = scalib_object$pred_horizon,
          cov = ._pred_lnlc_.,
          fit = hare_fit
        )
      )

    ._error_. <- abs(._obs_. - ._pred_.)

    hare_result <- data.table(
      ._id_. = pred_risk_cols[i],
      hare_ici   = base::mean(._error_.),
      hare_e50   = stats::median(._error_.),
      hare_e90   = stats::quantile(._error_., probs = 9/10),
      hare_emax  = base::max(._error_.)
    )

    dt_smry <- data.table(
      observed = ._obs_.,
      predicted = ._pred_.,
      abs_error = ._error_.
    )

    risk_grid_domain <- stats::quantile(._pred_., probs = c(0.01, 0.99))

    dt_plot <- data.table(
      predicted = seq(risk_grid_domain[1],
                      risk_grid_domain[2],
                      length.out = 500)
    )

    dt_plot$observed <-
      do.call(
        polspline::phare,
        args = list(
          q = scalib_object$pred_horizon,
          cov = log_neg_log_complement(dt_plot$predicted),
          fit = hare_fit
        )
      )

    hare_result$hare_data_plot <- list(dt_plot)
    hare_result$hare_data_smry <- list(dt_smry)

    result[[i]] <- hare_result

  }

  scalib_absorb(scalib_object, rbindlist(result))

}



# dt_unnest <- function (dt_, col, ...) {
#  if (isFALSE(is.data.table(dt_)))
#   dt_ <- as.data.table(dt_)
#  col <- substitute(col)
#  keep <- substitute(alist(...))
#  names <- colnames(dt_)
#  others <- names[-match(paste(col), names)]
#  rows <- sapply(dt_[[paste(col)]], NROW)
#  if (length(keep) > 1)
#   others <- others[others %in% paste(keep)[-1]]
#  others_dt <- dt_[, ..others]
#  classes <- sapply(others_dt, typeof)
#  keep <- names(classes)[classes != "list"]
#  others_dt <- others_dt[, ..keep]
#  others_dt <- lapply(others_dt, rep, times = rows)
#  dt_[, list(as.data.table(others_dt), rbindlist(eval(col)))]
# }









