

all_equal <- function(x){diff(range(x)) < .Machine$double.eps^0.5}
