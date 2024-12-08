library(data.table)
library(readr)
input <- data.table(read_delim("~/Documents/advent of code/advent of code 2024/input_day8.txt",col_names = FALSE,delim = "thiswillnotappear",skip_empty_rows = TRUE))
rows <- nrow(input)
cols <- nchar(input[1]$X1)
cols_list <- 1:cols

unique_satelites <- c("0")
satelites <- data.table(sat_sym = c(),sat_row = c(),sat_col = c())
antinodes <- data.table(node_row = c(),node_col = c())
antinodes_pt2 <- data.table(node_row = c(),node_col = c())

for(row in 1:rows) {
  map_row <- strsplit(input[row]$X1,split = "")[[1]]
  satelite_cols <- cols_list[map_row != "."]
  satelite_syms <- map_row[map_row != "."]
  if(length(satelite_cols) > 0) {
    satelites <- rbind(satelites,data.table(sat_sym = c(satelite_syms), sat_row = c(row),sat_col = c(satelite_cols)))
    unique_satelites <- unique(c(unique_satelites,satelite_syms))
  }
}

for(satelite_type in 1:length(unique_satelites)) {
  curr_sat_type <- unique_satelites[satelite_type]
  curr_sat_locations <- satelites[sat_sym == curr_sat_type]
  sat_pairs <- data.table(t(combn(nrow(curr_sat_locations),2)))
  for(pair in 1:nrow(sat_pairs)) {
    sat_one <- curr_sat_locations[sat_pairs[pair]$V1]
    sat_two <- curr_sat_locations[sat_pairs[pair]$V2]
    row_diff <- sat_two$sat_row - sat_one$sat_row
    col_diff <- sat_two$sat_col - sat_one$sat_col
    
    ### part 1 antinodes
    nodes_rows <- c(sat_two$sat_row+row_diff,sat_one$sat_row-row_diff)
    nodes_cols <- c(sat_two$sat_col+col_diff,sat_one$sat_col-col_diff)
    antinodes <- rbind(antinodes,data.table(node_row = nodes_rows,node_col = nodes_cols))
    
    ### part 2 antinodes
      ### add nodes to one side
      node_in_bounds <- TRUE
      node_row_curr <- sat_two$sat_row
      node_col_curr <- sat_two$sat_col
      while (node_in_bounds) {
        if(node_row_curr > 0 & node_row_curr <= rows & node_col_curr > 0 & node_col_curr <= cols) {
          antinodes_pt2 <- rbind(antinodes_pt2,data.table(node_row = node_row_curr,node_col = node_col_curr))
          node_row_curr <- node_row_curr+row_diff
          node_col_curr <- node_col_curr+col_diff
        } else {
          node_in_bounds <- FALSE
        }
      }
    
      ### add nodes to other side
      node_in_bounds <- TRUE
      node_row_curr <- sat_one$sat_row
      node_col_curr <- sat_one$sat_col
      while (node_in_bounds) {
        if(node_row_curr > 0 & node_row_curr <= rows & node_col_curr > 0 & node_col_curr <= cols) {
          antinodes_pt2 <- rbind(antinodes_pt2,data.table(node_row = node_row_curr,node_col = node_col_curr))
          node_row_curr <- node_row_curr-row_diff
          node_col_curr <- node_col_curr-col_diff
        } else {
          node_in_bounds <- FALSE
        }
      }
    
  }
}

print(nrow(unique(antinodes[node_row > 0 & node_row <= rows & node_col > 0 & node_col <= cols])))
print(nrow(unique(antinodes_pt2)))

