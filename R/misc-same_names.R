same_names <- function(x,y){

 all(names(x) %in% names(y)) && all(names(y) %in% names(x))

}
