
log_neg_log_complement <- function(x){

 x[x==1] <- 0.9999
 x[x==0] <- 0.0001

 log(-log(1 - x))

}
