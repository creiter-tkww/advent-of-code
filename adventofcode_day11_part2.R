library(data.table)
library(readr)
input <- c(t(read_delim("~/Documents/advent of code/advent of code 2024/input_day11.txt",col_names = FALSE,delim = " ",skip_empty_rows = TRUE)))
all_blinks_table <- data.table(num = c("0"),blinks_remaining = c(0),stones = c(1))

split_string <- function(string) {
  length_string <- nchar(string)
  half1 <- substring(string,1,length_string/2)
  half2 <- substring(string,length_string/2+1,length_string)
  return(c(half1,half2))
}

stones_created <- function(stone_num,blinks,all_blinks_table) {
  if(blinks == 0) {
    return(list(1,all_blinks_table))
  } else if (nrow(all_blinks_table[num == stone_num & blinks_remaining == blinks]) > 0) {
    return(list(all_blinks_table[num == stone_num & blinks_remaining == blinks][1]$stones,all_blinks_table))
  } else {
    if(stone_num == "0") {
      output <- stones_created("1",blinks-1,all_blinks_table)
      stone_count <- output[[1]]
      all_blinks_table <- output[[2]]
      all_blinks_table <- rbind(all_blinks_table,data.table(stone_num,blinks,stone_count),use.names = FALSE)
      return(list(stone_count,all_blinks_table))
    } else if (nchar(stone_num)%%2 == 0) {
      new_stones <- as.character(as.numeric(split_string(stone_num)))
      output1 <- stones_created(new_stones[1],blinks-1,all_blinks_table)
      output2 <- stones_created(new_stones[2],blinks-1,all_blinks_table)
      stone_count <- output1[[1]]+output2[[1]]
      all_blinks_table <- unique(rbind(output1[[2]],output2[[2]]))
      all_blinks_table <- rbind(all_blinks_table,data.table(stone_num,blinks,stone_count),use.names = FALSE)
      return(list(stone_count,all_blinks_table))
    } else {
      output <- stones_created(as.character((as.numeric(stone_num)*2024)),blinks-1,all_blinks_table)
      stone_count <- output[[1]]
      all_blinks_table <- output[[2]]
      all_blinks_table <- rbind(all_blinks_table,data.table(stone_num,blinks,stone_count),use.names = FALSE)
      return(list(stone_count,all_blinks_table))
    }
  }
}


for(blink in 1:75) {
input_table <- data.table(num = input,blinks_remaining = blink)

for(row in 1:nrow(input_table)) {
  output <- stones_created(input_table[row]$num,input_table[row]$blinks_remaining,all_blinks_table)
  row_stones_created <- output[[1]]
  all_blinks_table <- output[[2]]
  input_table[row,stones := row_stones_created]
}

all_blinks_table <- unique(all_blinks_table)

}
sprintf("%.0f",sum(input_table$stones))
