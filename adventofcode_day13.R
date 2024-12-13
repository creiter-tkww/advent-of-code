library(data.table)
library(readr)
input <- data.table(read_delim("~/Documents/advent of code/advent of code 2024/input_day13.txt",col_names = FALSE,delim = ":",skip_empty_rows = TRUE))
max_prizes <- nrow(input[X1 == "Prize"])
total_cost <- 0
tol <- .001

for(possible_prize in 1:max_prizes) {
  button_A <- as.numeric(strsplit(input[possible_prize*3-2]$X2,split = "\\W")[[1]])
  button_B <- as.numeric(strsplit(input[possible_prize*3-1]$X2,split = "\\W")[[1]])
  prize <- as.numeric(strsplit(input[possible_prize*3]$X2,split = "\\W")[[1]])
  
  matrix_buttons <- matrix(c(button_A[3],button_A[6],button_B[3],button_B[6]),2,2)
  totals <- c(prize[3],prize[6])
  answer <- solve(matrix_buttons,totals)
  
  if(answer[1] >= 0 & answer[2] >= 0 & answer[1] <= 100 & answer[2] <= 100 & min(abs(c(answer[1]%%1, answer[1]%%1-1))) < tol & min(abs(c(answer[2]%%1, answer[2]%%1-1))) < tol) {
    total_cost <- answer[1]*3 + answer[2]*1 + total_cost
  }
  
}

print(total_cost)

total_cost_part_2 <- 0

for(possible_prize in 1:max_prizes) {
  button_A <- as.numeric(strsplit(input[possible_prize*3-2]$X2,split = "\\W")[[1]])
  button_B <- as.numeric(strsplit(input[possible_prize*3-1]$X2,split = "\\W")[[1]])
  prize <- as.numeric(strsplit(input[possible_prize*3]$X2,split = "\\W")[[1]])
  
  matrix_buttons <- matrix(c(button_A[3],button_A[6],button_B[3],button_B[6]),2,2)
  totals <- c(prize[3],prize[6])+10000000000000
  answer <- solve(matrix_buttons,totals)
  
  if(answer[1] >= 0 & answer[2] >= 0 & min(abs(c(answer[1]%%1, answer[1]%%1-1))) < tol & min(abs(c(answer[2]%%1, answer[2]%%1-1))) < tol) {
    total_cost_part_2 <- answer[1]*3 + answer[2]*1 + total_cost_part_2
  }
  
}

sprintf("%.0f",total_cost_part_2)

