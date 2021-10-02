

lump_group <- function(group, variable, events, min_size = 5){

 index_lowest_count <- which.min(events$N)

 index_new_group <-
  if(index_lowest_count == 1){
   2
  } else if (index_lowest_count == nrow(events)){
   nrow(events) - 1
  } else {
   find_new_group_index(events, index_lowest_count)
  }

 replace(group,
         list = group == events$group[index_lowest_count],
         values = events$group[index_new_group])

}

find_new_group_index <- function(events, index_lowest_count){

 use_plus_1 <-
  events$N[index_lowest_count + 1] < events$N[index_lowest_count - 1]

 if(use_plus_1){
  index_lowest_count + 1
 } else {
  index_lowest_count - 1
 }

}
