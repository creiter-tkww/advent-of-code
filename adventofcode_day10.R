library(data.table)
library(readr)
input <- data.table(read_delim("~/Documents/advent of code/advent of code 2024/input_day10.txt",col_names = FALSE,delim = "thiswillnotappear",skip_empty_rows = TRUE))
rows <- nrow(input)
cols <- nchar(input[1]$X1)

### create a table with all grid information
map_in_table_format <- data.table(row_num = c(),col_num = c(),height = c())
for(row in 1:rows) {
  full_row_split <- strsplit(input[row]$X1,split = "")[[1]]
  for(col in 1:cols) {
    map_in_table_format <- rbind(map_in_table_format,data.table(row_num = c(row),col_num = c(col),height = c(full_row_split[col])))
  }
}

map_in_table_format <- map_in_table_format[order(height)]

running_total_scores <- 0
running_total_ratings <- 0

for(starting_location in 1:nrow(map_in_table_format[height == 0])) {
  map_from_current_start <- rbind(map_in_table_format[height == 0][starting_location],map_in_table_format[height != 0])
  map_from_current_start[,valid_path := ifelse(height == 0,1,0)]
  map_from_current_start[,valid_paths := ifelse(height == 0,1,0)]
  for(height_current in 0:8) {
    coords <- map_from_current_start[height == height_current & valid_path == 1]
    for(coord_current in 1:nrow(coords)) {
      row_current <- coords[coord_current]$row_num
      col_current <- coords[coord_current]$col_num
      paths_current <- coords[coord_current]$valid_paths
      map_from_current_start[height == height_current+1 & abs(row_num-row_current)+ abs(col_num-col_current)<=1,valid_path := 1]
      map_from_current_start[height == height_current+1 & abs(row_num-row_current)+ abs(col_num-col_current)<=1,valid_paths := valid_paths+paths_current]
    }
  }
  running_total_scores <- running_total_scores+nrow(map_from_current_start[height == 9 & valid_path == 1])
  running_total_ratings <- running_total_ratings+sum(map_from_current_start[height == 9 & valid_path == 1]$valid_paths)
}

print(running_total_scores)
print(running_total_ratings)
