

#' Print Calibration Test Results
#'
#' @param x an object of class `survival_calib_test`
#' @param ... additional arguments. Eligible inputs are:
#'  - __digits__: minimal number of significant digits to print.
#'  - __max_rows_to_print__: maximum number of rows to print
#'
#'
#' @return nothing. Output is printed to console.
#' @export
#'
print.survival_calib_test <- function(x, ...){

  .dots <- list(...)

  if(is.null(.dots$digits))
    digits <- 3
  else
    digits <- .dots$digits
  if(is.null(.dots$max_rows_to_print))
    max_rows_to_print <- 5
  else
    max_rows_to_print <- .dots$max_rows_to_print

  if(max_rows_to_print < nrow(x$statistic))
    x_statistic_header <-
    paste("\n- Greenwood-Nam-D'Agostino (GND) Test at t =",
          x$time_predict, "(truncated):\n")
  else
    x_statistic_header <-
    paste0("\n- Greenwood-Nam-D'Agostino (GND) Test at t = ",
           x$time_predict, ":\n")

  if(nrow(x$statistic) == 1)
    cat(
      paste0(
        "-------------------------------------------------------\n",
        x_statistic_header,
        "\n-- Chi-square statistic: ", round(x$statistic$GND_chisq, digits),
        "\n-- degrees of freedom: ", x$statistic$GND_df,
        "\n-- P-value for miscalibration: ",
        table.glue::table_pvalue(x$statistic$GND_pvalue),
        "\n-- Warnings: ", x$warnings, "\n",
        "\n-------------------------------------------------------\n"
      )
    )

  if(nrow(x$statistic) >= 2){

    cat("\n-------------------------------------------------------\n",
        x_statistic_header)

    print(x$statistic,
          digits = digits,
          row.names = FALSE)

    cat("\n---------------------------------------------------------\n")

  }

 data_to_print <- x$data
 data_to_print$events_observed <- NULL
 data_to_print$events_expected <- NULL
 data_to_print$variance <- NULL
 data_to_print$GND_component <- NULL

 if(max_rows_to_print < nrow(data_to_print))
  msg <- paste("\n- Events data by group at t =",
               x$time_predict, "(truncated):\n\n")
 else
  msg <- paste0("\n- Events data by group at t = ",
                x$time_predict, ":\n\n")

 cat(msg)
 print(data_to_print[seq(max_rows_to_print), ],
       digits = digits,
       row.names = FALSE)


}
