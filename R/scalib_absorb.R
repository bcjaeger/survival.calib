

scalib_absorb <- function(x, y){

 # y is absorbed into x
 # only use this if x and y have the same input data


 # good ole' CRAN
 ._id_. = NULL

 if(is_scalib(y)){
  stopifnot(all.equal(x$data_inputs, y$data_inputs))
  y <- y$data_outputs
 }

 stopifnot(is.data.frame(y))

 if(is_empty(names(x$data_outputs))){
  x$data_outputs <- y
  setkey(x$data_outputs, ._id_.)
  return(x)
 }

 names_are_equal <-
  same_names(x$data_outputs, y)

 names_are_disjoint <-
  length(intersect(names(x$data_outputs), names(y))) == 1

 if(!names_are_equal && !names_are_disjoint)
  browser()
 # stop("unable to combine scalib objects", call. = FALSE)

 if(names_are_disjoint){
  return(output_merge(x,y))
 }

 if(names_are_equal){
  return(output_rbind(x,y))
 }

}

#
# scalib_absorb <- function(x, y){
#
#   # y is absorbed into x
#   # only use this if x and y have the same input data
#
#   if(is_scalib(y)){
#     stopifnot(all.equal(x$data_inputs, y$data_inputs))
#     y <- y$data_outputs
#   }
#
#
#   stopifnot(is.data.frame(y))
#
#   names_x <- names(x$data_outputs)
#
#   if(is_empty(names_x)){
#     x$data_outputs <- y
#     setkey(x$data_outputs, ._id_.)
#     return(x)
#   }
#
#   browser()
#
#   names_y <- names(y)
#   names_x_in_y <- all(names_x %in% names_y)
#   names_y_in_x <- all(names_y %in% names_x)
#   names_equal <- names_x_in_y && names_y_in_x
#   names_intersect <- intersect(names_x, names_y)
#
#   if(is_empty(names_intersect))
#     stop("both objects should have a ._id_. column",
#          call. = FALSE)
#
#   names_disjoint <- all(names_intersect == '._id_.')
#
#   prisk_x <- x$data_outputs$._id_.
#   prisk_y <- y$._id_.
#   prisk_x_in_y <- all(prisk_x %in% prisk_y)
#   prisk_y_in_x <- all(prisk_y %in% prisk_x)
#   prisk_equal <- prisk_x_in_y && prisk_y_in_x
#   prisk_disjoint <- is_empty(intersect(prisk_x, prisk_y))
#
#
#   # if y has the same columns as x and different predrisk, rbind
#   if(names_equal && prisk_disjoint)
#     return(output_rbind(x,y))
#
#   # if y has the same columns as x and the same predrisk, error
#   if(names_equal && prisk_equal)
#     stop("x and y are the same (?)", call. = FALSE)
#   # if y has different columns as x and the same predrisk, merge
#
#   if(names_disjoint && prisk_y_in_x)
#     return(output_merge(x, y))
#
#   # if y has different columns as x and different predrisk, error
#   if(names_disjoint && prisk_disjoint)
#     return(output_rbind(x, y))
#
#   # if y columns are contained in x and different predrisk, rbind
#   if(names_y_in_x && prisk_disjoint)
#     return(output_rbind(x, y))
#
#   # if y columns are contained in x and the same predrisk, use set
#   if(names_y_in_x && prisk_y_in_x)
#     return(output_splice(x, y))
#
#   browser()
#   stop("unable to combine scalib objects", call. = FALSE)
#
#
# }
#
# output_splice <- function(x, y){
#
#   i <- which(x$data_outputs$._id_. == y$._id_.)
#
#   for(j in setdiff(names(y), '._id_.')){
#     set(
#       x = x$data_outputs,
#       i = i,
#       j = j,
#       value = y[[j]]
#     )
#   }
#
#   x
#
# }
#
#


output_rbind <- function(x, y, fill = TRUE){
 x$data_outputs <- rbind(x$data_outputs,
                         y,
                         fill = fill)
 x
}

output_merge <- function(x, y){

 x$data_outputs <-
  merge(x = x$data_outputs,
        y = y,
        all.x = TRUE,
        by = '._id_.')

 x

}
