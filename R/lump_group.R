

lump_group <- function(group, events, min_size = 5){

 index_lowest_count <- which.min(events$Freq)

 if(index_lowest_count == 1){
  index_new_group <- 2
 } else {
  index_new_group <- index_lowest_count - 1
 }

 replace(group,
         list = group == events$group[index_lowest_count],
         values = events$group[index_new_group])

}
