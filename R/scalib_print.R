

#' Print Calibration Test Results
#'
#' @param x an object of class `scalib_test`
#' @param ... additional arguments. Eligible inputs are:
#'  - __digits__: minimal number of significant digits to print.
#'  - __max_rows_to_print__: maximum number of rows to print
#'
#'
#' @return nothing. Output is printed to console.
#' @export
#'
print.scalib <- function(x, ...){

  .dots <- list(...)

  if(is.null(.dots$topn))       .dots$topn <- 5
  if(is.null(.dots$nrows))      .dots$nrows <- 100
  if(is.null(.dots$class))      .dots$class <- TRUE
  if(is.null(.dots$row.names))  .dots$row.names <- TRUE
  if(is.null(.dots$col.names))  .dots$col.names <- "auto"
  if(is.null(.dots$print.keys)) .dots$print.keys <- TRUE
  if(is.null(.dots$digits))     .dots$digits <- 3

  .dots$trunc.cols <- TRUE

  .dots$x <- x$data_inputs

  banner_input_length <-
    max(vapply(capture.output(do.call(print, .dots)), nchar, integer(1)))
    #nchar(capture.output(do.call(print, .dots))[1])

  banner_input <- paste(
    rep("-", times = banner_input_length - nchar("-- Input data ")),
    collapse = ''
  )

  cat("\n",
      "Survival calibration object with prediction horizon of ",
      x$pred_horizon,
      "\n\n",
      "-- Input data ",
      banner_input,
      "\n\n",
      sep = "")

  do.call(print, args = .dots)

  if(!is_empty(x$data_outputs)){

    .dots$x <- x$data_outputs

    banner_output_length <-
      max(vapply(capture.output(do.call(print, .dots)), nchar, integer(1)))

    banner_output <- paste(
      rep("-", times = banner_output_length - nchar("-- Output data ")),
      collapse = ''
    )

    cat("\n\n",
        "-- Output data ",
        banner_output,
        "\n\n",
        sep = "")

    do.call(print, args = .dots)

    cat("\n")

  }


}

