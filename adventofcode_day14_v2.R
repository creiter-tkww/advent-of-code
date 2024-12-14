library(data.table)
library(readr)
library(ggplot2)
input <- data.table(read_delim("~/Documents/advent of code/advent of code 2024/input_day14.txt",col_names = FALSE,delim = " ",skip_empty_rows = TRUE))
rows <- 103
cols <- 101

robot_table <- data.table(start_row = 0,start_col = 0,vel_row = 0,vel_col = 0)
for (robot in 1:nrow(input)) {
  robot_start <- as.numeric(strsplit(substring(input[robot]$X1,3,nchar(input[robot]$X1)),split = ",")[[1]])
  robot_vel <- as.numeric(strsplit(substring(input[robot]$X2,3,nchar(input[robot]$X2)),split = ",")[[1]])
  robot_table <- rbind(robot_table,data.table(start_row = 0,start_col = 0,vel_row = 0,vel_col = 0))
  robot_table[robot]$start_row <- robot_start[2]
  robot_table[robot]$start_col <- robot_start[1]
  robot_table[robot]$vel_row <- robot_vel[2]
  robot_table[robot]$vel_col <- robot_vel[1]
}

robot_table <- robot_table[1:500]

pt_1_ans <- 0

for(moves in 1:(101*103)) {
  robot_table[,col := (start_col+vel_col*moves)%%cols]
  robot_table[,row := (start_row+vel_row*moves)%%rows]
  
  if(nrow(robot_table[col == 62]) > 30 & nrow(robot_table[col == 32]) > 30) {
    print(ggplot(robot_table,aes(x = row,y=col)) + geom_point() + labs(title = moves))
  }
  
  if(moves == 100) {
    robot_table[,quad := ifelse(col+1 < cols/2 & row+1 < rows/2,1,ifelse(col+1 < cols/2 & row > rows/2,2,ifelse(
      col > cols/2 & row > rows/2,3,ifelse(col > cols/2 & row+1 < rows/2,4,0))))]
    pt_1_ans <- nrow(robot_table[quad == 1])*nrow(robot_table[quad == 2])*nrow(robot_table[quad == 3])*nrow(robot_table[quad == 4])
  }

}

print(pt_1_ans)