

#' Print GND Test Results
#'
#' @param x an object of class `survival.calib_gnd_test`
#' @param ... additional arguments. Eligible inputs are:
#'  - __digits__: minimal number of significant digits to print.
#'  - __max_rows_to_print__: maximum number of rows to print
#'
#'
#' @return nothing. Output is printed to console.
#' @export
#'
print.survival.calib_gnd_test <- function(x, ...){

  .dots <- list(...)

  if(is.null(.dots$digits))
    digits <- 3
  else
    digits <- .dots$digits
  if(is.null(.dots$max_rows_to_print))
    max_rows_to_print <- 5
  else
    max_rows_to_print <- .dots$max_rows_to_print

 msg <- paste0(
  "-----------------------------------------------------\n",
  "\n- Greenwood-Nam-D'Agostino (GND) Goodness-of-Fit Test\n",
  "\n-- Chi-square test statistic: ", round(x$statistic$GND_chisq, digits),
  "\n-- degrees of freedom: ", x$statistic$GND_df,
  "\n-- P-value for lack of fit: ",
  table.glue::table_pvalue(x$statistic$GND_pvalue),
  "\n-- Warnings: ", x$warnings, "\n",
  "\n-----------------------------------------------------\n"
 )


 data_to_print <- x$data[, c("group_label",
                             "group_n",
                             "events_observed",
                             "events_expected")]

 if(max_rows_to_print < nrow(data_to_print))
  msg <- paste(msg, "\n- Events data by group (truncated):\n\n")
 else
  msg <- paste(msg, "\n- Events data by group:\n\n")

 within(
  data = data_to_print,
  expr = {
   events_expected = round(events_expected, digits)
  }
 )

 cat(msg)
 print(data_to_print[seq(max_rows_to_print), ],
       digits = digits,
       row.names = FALSE)

 if(max_rows_to_print < nrow(data_to_print))
  cat("\n+", nrow(data_to_print) - max_rows_to_print,"more rows")


}
