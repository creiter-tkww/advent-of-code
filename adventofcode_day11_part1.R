library(data.table)
library(readr)
input <- c(t(read_delim("~/Documents/advent of code/advent of code 2024/input_day11.txt",col_names = FALSE,delim = " ",skip_empty_rows = TRUE)))

input <- c("0")
blinks <- 25

split_string <- function(string) {
  length_string <- nchar(string)
  half1 <- substring(string,1,length_string/2)
  half2 <- substring(string,length_string/2+1,length_string)
  return(c(half1,half2))
}

for(blink in 1:blinks) {
  ### find the stones in each category
  indexes_zeros <- input == "0"
  indexes_even_digits <- nchar(input)%%2 == 0
  stones_even_digits <- input[indexes_even_digits]
  stones_other <- input[!indexes_zeros & !indexes_even_digits]
  ### update the stones
  stones_one <- rep("1",sum(indexes_zeros))
  stones_even_digits_split <- as.numeric(unlist(lapply(stones_even_digits,split_string)))
  stones_mult <- as.numeric(stones_other)*2024
  ### put the stones together into one vector
  input <- as.character(c(stones_one,stones_even_digits_split,stones_mult))
}


print(length(input))
