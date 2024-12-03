library(data.table)
library(readr)
input <- data.table(read_delim("~/Documents/advent of code/advent of code 2024/input_day3.txt",col_names = FALSE,delim = "thiswillneverappear",skip_empty_rows = FALSE))

total <- 0
total_enabled <- 0
digits <- 0:9
enabled <- TRUE

for (row in 1:nrow(input)) {
  input_split <- strsplit(input[row]$X1,split = "mul")[[1]]
    for (mul_string_row in 1:length(input_split)) {
      mul_string <- input_split[mul_string_row]
      if(substring(mul_string,1,1) == "(") {
        nums <- strsplit(substring(mul_string,2,nchar(mul_string)),split = ")")[[1]][1]
        nchar_nums <- nchar(nums)
        if(gsub("\\d","",nums)=="," & substring(nums,1,1) %in% digits  & substring(nums,nchar_nums,nchar_nums) %in% digits) {
          nums_split <- as.numeric(strsplit(nums,",")[[1]])
          total <- total + nums_split[1]*nums_split[2]
          if(enabled) {
            total_enabled <- total_enabled + nums_split[1]*nums_split[2]
          }
        }
      }
      
      ##part two enabled/disabled
      if(mul_string %like% "do\\(\\)" & !mul_string %like% "don't\\(\\)") {
        enabled <- TRUE
      } else if(!mul_string %like% "do\\(\\)" & mul_string %like% "don't\\(\\)"){
        enabled <- FALSE
      } else if(mul_string %like% "do\\(\\)" & mul_string %like% "don't\\(\\)") {
        print("how many times do I have this use case")
      }
  }
  
}
print(total)
print(total_enabled)