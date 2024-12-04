library(data.table)
library(readr)
input <- data.table(read_delim("~/Documents/advent of code/advent of code 2024/input_day4.txt",col_names = FALSE,delim = "thiswillneverappear",skip_empty_rows = FALSE))
rows <- nrow(input)
cols <- nchar(input[1]$X1)

xmas_count <- 0
for(row in 1:rows) {
  curr_row <- input[row]$X1
  curr_row_split <- strsplit(curr_row,split = "")[[1]]
  x_locations <- grep("X",curr_row_split)
  count_x_locations <- length(x_locations)
  for (x_location in 1:count_x_locations) {
      curr_x_col <- x_locations[x_location]
      possible_ms <- list(c(row,curr_x_col-1),c(row-1,curr_x_col-1),c(row-1,curr_x_col),
                         c(row-1,curr_x_col+1),c(row,curr_x_col+1),c(row+1,curr_x_col+1),
                         c(row+1,curr_x_col),c(row+1,curr_x_col-1))
      possible_as <- list(c(row,curr_x_col-2),c(row-2,curr_x_col-2),c(row-2,curr_x_col),
                          c(row-2,curr_x_col+2),c(row,curr_x_col+2),c(row+2,curr_x_col+2),
                          c(row+2,curr_x_col),c(row+2,curr_x_col-2))
      possible_ss <- list(c(row,curr_x_col-3),c(row-3,curr_x_col-3),c(row-3,curr_x_col),
                          c(row-3,curr_x_col+3),c(row,curr_x_col+3),c(row+3,curr_x_col+3),
                          c(row+3,curr_x_col),c(row+3,curr_x_col-3))
      for(try in 1:8) {
        m_location_try <- possible_ms[[try]]
        a_location_try <- possible_as[[try]]
        s_location_try <- possible_ss[[try]]
        if(sum(s_location_try > 0 ) == 2 & sum(s_location_try < rows+1) == 2) {
          is_m <- substring(input[m_location_try[1]]$X1,m_location_try[2],m_location_try[2]) == "M"
          if(is_m) {
            is_a <- substring(input[a_location_try[1]]$X1,a_location_try[2],a_location_try[2]) == "A"
            if(is_a) {
              is_s <- substring(input[s_location_try[1]]$X1,s_location_try[2],s_location_try[2]) == "S"
              if(is_s) {
                xmas_count <- xmas_count + 1
              }
            }
          }
        }
      }
  }
}

print(xmas_count)

x_mas_count <- 0
for(row in 1:rows) {
  curr_row <- input[row]$X1
  curr_row_split <- strsplit(curr_row,split = "")[[1]]
  m_locations <- grep("M",curr_row_split)
  count_m_locations <- length(m_locations)
  for (m_location in 1:count_m_locations) {
    curr_m_col <- m_locations[m_location]
    possible_second_ms <- list(c(row,curr_m_col-2),c(row-2,curr_m_col),c(row,curr_m_col+2),
                        c(row+2,curr_m_col))
    possible_as <- list(c(row-1,curr_m_col-1),c(row-1,curr_m_col+1),c(row+1,curr_m_col+1),
                        c(row+1,curr_m_col-1))
    possible_ss <- list(c(row-2,curr_m_col),c(row,curr_m_col+2),c(row+2,curr_m_col),
                        c(row,curr_m_col-2))
    possible_second_ss <- list(c(row-2,curr_m_col-2),c(row-2,curr_m_col+2),c(row+2,curr_m_col+2),
                        c(row+2,curr_m_col-2))
    for(try in 1:4) {
      second_m_location_try <- possible_second_ms[[try]]
      a_location_try <- possible_as[[try]]
      s_location_try <- possible_ss[[try]]
      second_s_location_try <- possible_second_ss[[try]]
      if(sum(second_s_location_try > 0 ) == 2 & sum(second_s_location_try < rows+1) == 2) {
        is_second_m <- substring(input[second_m_location_try[1]]$X1,second_m_location_try[2],second_m_location_try[2]) == "M"
        if(is_second_m) {
          is_a <- substring(input[a_location_try[1]]$X1,a_location_try[2],a_location_try[2]) == "A"
          if(is_a) {
            is_s <- substring(input[s_location_try[1]]$X1,s_location_try[2],s_location_try[2]) == "S"
            if(is_s) {
              is_second_s <- substring(input[second_s_location_try[1]]$X1,second_s_location_try[2],second_s_location_try[2]) == "S"
              if(is_second_s) {
                x_mas_count <- x_mas_count + 1
              }
            }
          }
        }
      }
    }
  }
}

print(x_mas_count)