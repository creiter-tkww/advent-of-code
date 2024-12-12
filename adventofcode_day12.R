library(data.table)
library(readr)
input <- data.table(read_delim("~/Documents/advent of code/advent of code 2024/input_day12.txt",col_names = FALSE,delim = "thiswillnotappear",skip_empty_rows = TRUE))
rows <- nrow(input)
cols <- nchar(input[1]$X1)

input_tabular <- CJ(row = 1:rows,col = 1:cols,symbol = "",region_num = -1,borders = -1,corners = 0)
for(curr_row in 1:rows) {
  input_curr_row <- strsplit(input[curr_row]$X1,split = "")[[1]]
  input_tabular[row == curr_row]$symbol <- input_curr_row
}

region_assign <- 1

find_surrounding_matches <- function(surrounding_input,all_matches) {
  surrounding_new <- data.table()
  for(x in 1:nrow(surrounding_input)) {
    surrounding_new <- rbind(surrounding_new,all_matches[abs(row - surrounding_input[x]$row) + abs(col - surrounding_input[x]$col) <= 1])
  }
  return(unique(surrounding_new))
}

get_surrounding_chars <- function(row_input,col_input,char,input_tabular) {
  dir1 <- input_tabular[row == row_input-1 & col == col_input]$symbol
  dir2 <- input_tabular[row == row_input-1 & col == col_input+1]$symbol
  dir3 <- input_tabular[row == row_input & col == col_input+1]$symbol
  dir4 <- input_tabular[row == row_input+1 & col == col_input+1]$symbol
  dir5 <- input_tabular[row == row_input+1 & col == col_input]$symbol
  dir6 <- input_tabular[row == row_input+1 & col == col_input-1]$symbol
  dir7 <- input_tabular[row == row_input & col == col_input-1]$symbol
  dir8 <- input_tabular[row == row_input-1 & col == col_input-1]$symbol
  syms <- list(dir1,dir2,dir3,dir4,dir5,dir6,dir7,dir8)
  matches <- unlist(lapply(syms,function(symbol) {
    if(length(symbol) > 0) {
      symbol == char
    } else {
      FALSE
    }
  }))
  return(matches)
}


for(curr_row in 1:rows) {
  for(curr_col in 1:cols) {
    input_current <- input_tabular[row == curr_row & col == curr_col]
    input_char <- input_current$symbol
    all_matches <- input_tabular[symbol == input_char]
    matches_region_max <- max(all_matches$region_num)
    surrounding <- find_surrounding_matches(input_current,all_matches)
    input_tabular[row == curr_row & col == curr_col,borders := 5-nrow(surrounding)]
    if (nrow(surrounding) == 1 | matches_region_max == -1) {
      input_tabular[row == curr_row & col == curr_col,region_num := region_assign]
      region_assign <- region_assign+1
    } else if (input_current$region == -1) {
      region <- max(surrounding$region_num)
      found_full_region <- FALSE
      while(region == -1 & !found_full_region) {
        region_area <- nrow(surrounding)
        surrounding <- find_surrounding_matches(surrounding,all_matches)
        found_full_region <- region_area == nrow(surrounding)
        region <- max(surrounding$region_num)
      }
      region_all <- paste0(surrounding$row,",",surrounding$col)
      if(region == -1) {
        input_tabular[(row==curr_row & col == curr_col) | paste0(row,",",col) %in% region_all,region_num := region_assign]
        region_assign <- region_assign+1
      } else {
        input_tabular[(row==curr_row & col == curr_col) | paste0(row,",",col) %in% region_all,region_num := region]
      }
    }
    
    ## part 2 - calculate the corners (ie the sides)
    surrounding_chars <- get_surrounding_chars(curr_row,curr_col,input_char,input_tabular)
    corner_count <- sum(!surrounding_chars[1]&!surrounding_chars[3],
                        !surrounding_chars[3]&!surrounding_chars[5],
                        !surrounding_chars[5]&!surrounding_chars[7],
                        !surrounding_chars[7]&!surrounding_chars[1],
                        surrounding_chars[1]&surrounding_chars[3]&!surrounding_chars[2],
                        surrounding_chars[3]&surrounding_chars[5]&!surrounding_chars[4],
                        surrounding_chars[5]&surrounding_chars[7]&!surrounding_chars[6],
                        surrounding_chars[7]&surrounding_chars[1]&!surrounding_chars[8])
    input_tabular[row==curr_row & col == curr_col,corners := corner_count]
  }
}


total <- input_tabular[,list(sum(borders),.N),list(region_num)][,list(sum(V1*N)),list()]
print(total)
total2 <- input_tabular[,list(sum(corners),.N),list(region_num)][,list(sum(V1*N)),list()]
print(total2)
