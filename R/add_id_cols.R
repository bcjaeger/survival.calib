


add_id_cols <- function(calib_object, id_value){

 UseMethod('add_id_cols')

}

add_id_cols.survival_calib_slope <- function(calib_object, id_value){

 ids_already_there <-
  'id' %in% names(calib_object$data) &&
  'id' %in% names(calib_object$summary)

 if(ids_already_there) return(calib_object)

 data_id <- data.frame(id = id_value)

 calib_object$data <- cbind(data_id, calib_object$data)
 calib_object$summary <- cbind(data_id, calib_object$summary)
 calib_object

}

add_id_cols.survival_calib_test <- function(calib_object, id_value){

  ids_already_there <-
    'id' %in% names(calib_object$data) &&
    'id' %in% names(calib_object$statistic)

  if(ids_already_there) return(calib_object)

  data_id <- data.frame(id = id_value)

  calib_object$data <- cbind(data_id, calib_object$data)
  calib_object$statistic <- cbind(data_id, calib_object$statistic)
  calib_object


}
