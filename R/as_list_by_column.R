
as_list_by_column <- function(mat){

 .list <- vector(mode = 'list', length = ncol(mat))

 names(.list) <- colnames(mat)

 for(i in seq_along(.list)) .list[[i]] <- mat[, i]

 .list

}
