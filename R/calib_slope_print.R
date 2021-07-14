



#' Print Calibration Slope Results
#'
#' @param x an object of class `survival_calib_slope`
#' @param ... additional arguments. Eligible inputs are:
#'  - __digits__: minimal number of significant digits to print.
#'  - __max_rows_to_print__: maximum number of rows to print
#'
#'
#' @return nothing. Output is printed to console.
#' @export
#'
print.survival_calib_slope <- function(x, ...){

 .dots <- list(...)

 if(is.null(.dots$digits))
  digits <- 3
 else
  digits <- .dots$digits

 if(is.null(.dots$max_rows_to_print))
  max_rows_to_print <- 5
 else
  max_rows_to_print <- .dots$max_rows_to_print

 if(max_rows_to_print < nrow(x$data))
  x_data_header <- paste("\n- Observed and predicted risk at t =",
                          x$time_predict, "(truncated):\n\n")
 else
  x_data_header <- paste0("\n- Observed and predicted risk at t = ",
                          x$time_predict, ":\n\n")

 if(nrow(x$summary) == 1)
   cat(
     paste0(
       "-------------------------------------------------------\n",
       "\n- Absolute calibration error at t = ", x$time_predict, "\n",
       "\n-- mean, aka integrated calibration index (ICI): ", round(x$summary$ici, digits),
       "\n-- median (E50): ", round(x$summary$e50, digits),
       "\n-- 90th percentile (E90): ", round(x$summary$e90, digits),
       "\n-- maximum (EMax): ", round(x$summary$emax, digits),
       "\n\n-------------------------------------------------------\n"
     )
   )

 if(nrow(x$summary) >= 2){

   if(max_rows_to_print < nrow(x$summary))
     x_smry_header <- paste("\n- Calibration error summary at t =",
                            x$time_predict, "(truncated):\n\n")
   else
     x_smry_header <- paste0("\n- Calibration error summary at t = ",
                             x$time_predict, ":\n\n")

   cat("\n-------------------------------------------------------\n",
       x_smry_header)

   print(x$summary,
         digits = digits,
         row.names = FALSE)

   cat("\n-------------------------------------------------------\n")

 }

 cat(x_data_header)

 print(x$data[seq(max_rows_to_print), ],
       digits = digits,
       row.names = FALSE)


}
