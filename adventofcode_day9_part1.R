library(data.table)
library(readr)
input <- read_delim("~/Documents/advent of code/advent of code 2024/input_day9.txt",col_names = FALSE,delim = "thiswillnotappear",skip_empty_rows = TRUE,col_types = list("c"))$X1
id_num_count <- ceiling(nchar(input)/2)
id_numbers <- 0:(id_num_count-1)
input_split <- as.numeric(strsplit(input,split = "")[[1]])
blocks <- input_split[(1:id_num_count)*2-1]
spaces <- input_split[(1:(id_num_count-1))*2]
total_blocks <- sum(blocks)

input_num_current <- 1
max_id_current <- id_num_count-1
blocks_adjusted <- copy(blocks)
block_total_current <- blocks[1]
running_total <- 0

for (block in 1:total_blocks) {
  ### check if the current "block" is a space
  if(input_num_current %% 2 == 0) {
    ### it's a space - move up a number from the end
    running_total <- running_total + (block-1)*max_id_current
    count_max_id_remaining <- tail(blocks_adjusted,1)-1
    if(count_max_id_remaining > 0) {
      blocks_adjusted[length(blocks_adjusted)] <- count_max_id_remaining 
    } else {
      blocks_adjusted <- blocks_adjusted[1:(length(blocks_adjusted)-1)]
      max_id_current <- max_id_current-1
    }
  } else {
    ### it's not a space - add it to the running total
    running_total <- running_total + (block-1)*floor(input_num_current/2)
  }
  ### if at max, increase input_num_current
  if(block_total_current == block) {
    input_num_current <- input_num_current+1
    block_total_current <- block_total_current+input_split[input_num_current]
    if(block_total_current == block) {
      input_num_current <- input_num_current+1
      block_total_current <- block_total_current+input_split[input_num_current]
    }
  }
}

sprintf("%.0f",running_total)
