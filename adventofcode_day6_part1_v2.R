library(data.table)
library(readr)
input <- data.table(read_delim("~/Documents/advent of code/advent of code 2024/input_day6.txt",col_names = FALSE,delim = "thiswillneverappear",skip_empty_rows = TRUE))
rows <- nrow(input)
cols <- nchar(input[1]$X1)
obstacles <- data.table(obs_row = c(0),obs_col = c(0))
guard_loc <- c(0,0)
cols_list <- 1:cols

for(row in 1:rows) {
  map_row <- strsplit(input[row]$X1,split = "")[[1]]
  obstacles_cols <- cols_list[map_row == "#"]
  if(length(obstacles_cols) > 0) {
    obstacles <- rbind(obstacles,data.table(obs_row = c(row),obs_col = c(obstacles_cols)))
  }
  if(sum(map_row == "^") > 0) {
    guard_loc <- c(row,cols_list[map_row == "^"])
  }
}
obstacles <- obstacles[-1]

dirs <- 0:3
curr_dir <- 0
all_guard_locs <- data.table(guard_row = c(guard_loc[1]),guard_col = c(guard_loc[2]))
out_of_bounds <- FALSE

guard_move <- function(direction,location,obstacles) {
  out_of_bounds_move <- FALSE
  if(direction == 0) {
    next_obstacle <- obstacles[obs_row < location[1] & obs_col == location[2]]
    if(nrow(next_obstacle) > 0) {
      loc_new <- c(max(next_obstacle$obs_row)+1,location[2])
    } else {
      loc_new <- c(0,location[2])
      out_of_bounds_move <- TRUE
    }
  } else if (direction == 1) {
    next_obstacle <- obstacles[obs_row == location[1] & obs_col > location[2]]
    if(nrow(next_obstacle) > 0) {
      loc_new <- c(location[1],min(next_obstacle$obs_col)-1)
    } else {
      loc_new <- c(location[1],cols+1)
      out_of_bounds_move <- TRUE
    }
  } else if (direction == 2) {
    next_obstacle <- obstacles[obs_row > location[1] & obs_col == location[2]]
    if(nrow(next_obstacle) > 0) {
      loc_new <- c(min(next_obstacle$obs_row)-1,location[2])
    } else {
      loc_new <- c(rows+1,location[2])
      out_of_bounds_move <- TRUE
    }
  } else if (direction == 3) {
    next_obstacle <- obstacles[obs_row == location[1] & obs_col < location[2]]
    if(nrow(next_obstacle) > 0) {
      loc_new <- c(location[1],max(next_obstacle$obs_col)+1)
    } else {
      loc_new <- c(location[1],0)
      out_of_bounds_move <- TRUE
    }
  }
  return(list(loc_new,out_of_bounds_move))
}

while(!out_of_bounds) {
  move_output <- guard_move(curr_dir,guard_loc,obstacles)
  guard_loc_new <- move_output[[1]]
  out_of_bounds <- move_output[[2]]
  all_guard_locs <- rbind(all_guard_locs,data.table(guard_row = guard_loc[1]:guard_loc_new[1],guard_col = guard_loc[2]:guard_loc_new[2]))
  curr_dir <- ((curr_dir+1)%%4)
  guard_loc <- copy(guard_loc_new)
}

print(nrow(unique(all_guard_locs))-1)
all_guard_locs_pt_1 <- unique(all_guard_locs[1:(nrow(all_guard_locs)-1)])
