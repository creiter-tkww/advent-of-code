library(data.table)
library(readr)
input <- data.table(read_delim("~/Documents/advent of code/advent of code 2024/input_day1.txt",col_names = FALSE,delim = " ",skip_empty_rows = FALSE))

first_digits <- input$X1[order(input$X1)]
last_digits <- input$X4[order(input$X4)]

print(sum(abs(first_digits - last_digits)))

total_score <- 0
for (digit in 1:length(first_digits)) {
  curr_digit <- first_digits[digit]
  count_in_left <- sum(last_digits == curr_digit)
  total_score <- total_score + curr_digit*count_in_left
}

print(total_score)