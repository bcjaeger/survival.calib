boundary_enforce <- function(x, low=0.0001, high=0.9999){
 pmin(pmax(x, low), high)
}
