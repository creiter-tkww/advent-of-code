library(data.table)
library(readr)
input <- read_delim("~/Documents/advent of code/advent of code 2024/input_day9.txt",col_names = FALSE,delim = "thiswillnotappear",skip_empty_rows = TRUE,col_types = list("c"))$X1
id_num_count <- ceiling(nchar(input)/2)
id_numbers_desc <- (id_num_count-1):0
input_split <- as.numeric(strsplit(input,split = "")[[1]])
blocks <- input_split[(1:id_num_count)*2-1]
block_starts <- c()
spaces <- input_split[(1:(id_num_count-1))*2]
space_starts <- c()

running_total <- 0

### calculate the starting location of all numbered blocks and spaces
spaces_traversed <- 0
for(input_element in 1:length(input_split)) {
  if(input_element %% 2 == 1) {
    block_starts <- c(block_starts,spaces_traversed)
  } else {
    space_starts <- c(space_starts,spaces_traversed)
  }
  spaces_traversed <- spaces_traversed+input_split[input_element]
}


for(id_num in id_numbers_desc) {
  num_blocks <- tail(blocks,1)
  block_start <- tail(block_starts,1)
  ### see if there is a spot to move the top number
  options_to_move <- space_starts[spaces >= num_blocks]
  if(length(options_to_move) > 0) {
    ### if there is..
    ### calculate the new spot and add it to the running total
    new_location <- min(options_to_move)
    running_total <- running_total+sum(id_num*(new_location:(num_blocks+new_location-1)))
    ### adjust the starting location and length of the space it now takes up
    spaces[space_starts == new_location] <- spaces[space_starts == new_location]-num_blocks
    space_starts[space_starts == new_location] <- space_starts[space_starts == new_location]+num_blocks
  } else {
    ### if there is not, add it as-is to the running total
    running_total <- running_total+sum(id_num*(block_start:(block_start+num_blocks-1)))
  }  
  ### remove the last set of spaces and the last set of blocks
  blocks <- head(blocks,-1)
  block_starts <- head(block_starts,-1)
  spaces <- head(spaces,-1)
  space_starts <- head(space_starts,-1)
}

sprintf("%.0f",running_total)