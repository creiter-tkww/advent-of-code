library(data.table)
library(readr)
input <- data.table(read_delim("~/Documents/advent of code/advent of code 2024/input_day2.txt",col_names = FALSE,delim = ",",skip_empty_rows = FALSE))

safe <- 0
safe_dampener <- 0
for (input_row in 1:nrow(input)) {
  nums_vector <- as.numeric(strsplit(input[input_row]$X1,split = " ")[[1]])
  vector_diffs <- nums_vector[1:(length(nums_vector)-1)] - nums_vector[2:(length(nums_vector))]
  diffs_count <- length(vector_diffs)
  if(sum(-1 >= vector_diffs & vector_diffs >= -3) == diffs_count |
     sum(1 <= vector_diffs & vector_diffs <= 3) == diffs_count) {
    safe <- safe + 1
    safe_dampener <- safe_dampener + 1
  } else if (sum(-1 >= vector_diffs[-c(1)] & vector_diffs[-c(1)] >= -3) == diffs_count-1 |
             sum(1 <= vector_diffs[-c(1)] & vector_diffs[-c(1)] <= 3) == diffs_count-1) {
    safe_dampener <- safe_dampener + 1
  }
  else if (sum(-1 >= vector_diffs[-c(diffs_count)] & vector_diffs[-c(diffs_count)] >= -3) == diffs_count-1 |
           sum(1 <= vector_diffs[-c(diffs_count)] & vector_diffs[-c(diffs_count)] <= 3) == diffs_count-1) {
    safe_dampener <- safe_dampener + 1
  }
  else {
    can_be_dampened <- 0
    for (diff in 1:(length(vector_diffs)-1)) {
      new_diff <- vector_diffs[diff] + vector_diffs[diff+1]
      vector_diffs_dampened <- c(new_diff,vector_diffs[-c(diff,diff+1)])
      if(sum(-1 >= vector_diffs_dampened & vector_diffs_dampened >= -3) == length(vector_diffs_dampened) |
         sum(1 <= vector_diffs_dampened & vector_diffs_dampened <= 3) == length(vector_diffs_dampened)) {
        can_be_dampened <- 1
      }
    }
    safe_dampener <- safe_dampener + can_be_dampened
  }
}

print(safe)
print(safe_dampener)
