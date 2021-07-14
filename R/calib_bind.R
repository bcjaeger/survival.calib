


#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' # coming soon

calib_bind <- function(...){
 UseMethod('calib_bind')
}

#' @export
calib_bind.list <- function(...){

   if(length(list(...)) > 1)
      stop("calib_bind does not support multiple lists.",
           call. = FALSE)

   .dots <- c(...)
   .names <- names(.dots)
   if(is.null(.names)) .names <- seq(length(.dots))

   for(i in seq_along(.dots))
      .dots[[i]] <- add_id_cols(.dots[[i]], .names[i])

   Reduce(f = calib_bind, x = .dots)

}

#' @export
calib_bind.survival_calib_slope <- function(...){

 .dots <- list(...)

 if(is_empty(.dots)) return(NULL)

 check_bind_input(.dots)

 for(i in seq_along(.dots))
  .dots[[i]] <- add_id_cols(.dots[[i]], names(.dots)[i])

 survival_calib_slope_init(
       time_predict = .dots[[1]]$time_predict,
       data = reduce_to_tibble(.dots, 'data'),
       summary = reduce_to_tibble(.dots, 'summary')
    )

}

reduce_to_tibble <- function(x, element){
   as_tibble(Reduce(rbind, lapply(x, function(xx) xx[[element]])))
}


fill_names <- function(.list){

 if(is_empty(.list)) return(list())

 .names <- names(.list)

 if (is.null(.names)) {
  .names <- as.character(seq(length(.list)))
 }

 blank_name_index <- which(.names=='')

 if(!is_empty(blank_name_index))
  .names[blank_name_index] <- as.character(blank_name_index)

 names(.list) <- .names

 .list

}

check_bind_input <- function(.list){

 times <- vapply(X = .list,
                 FUN = function(x) x$time_predict,
                 FUN.VALUE = 0)

 if(!all_the_same(times))
  stop("cannot bind calib object that have different times of prediction",
       call. = FALSE)

 invisible()

}

#' @export
calib_bind.survival_calib_test <- function(...){

 .dots <- list(...)

 if(is_empty(.dots)) return(NULL)

 check_bind_input(.dots)

 for(i in seq_along(.dots))
  .dots[[i]] <- add_id_cols(.dots[[i]], names(.dots)[i])

 survival_calib_test_init(
    time_predict = .dots[[1]]$time_predict,
    statistic = reduce_to_tibble(.dots, 'statistic'),
    data = reduce_to_tibble(.dots, 'data'),
    warnings = unique(Reduce(c, lapply(.dots, function(x) x$warnings)))
 )

}


is_empty <- function(x) length(x) == 0

all_the_same <- function(x){diff(range(x)) < .Machine$double.eps^0.5}
