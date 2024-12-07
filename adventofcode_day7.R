library(data.table)
library(readr)
input <- data.table(read_delim("~/Documents/advent of code/advent of code 2024/input_day7.txt",col_names = FALSE,delim = ": ",skip_empty_rows = TRUE))

calculation_result <- function(numbers,symbols) {
  running_total <- numbers[1]
  for(symbol in 1:length(symbols)) {
    if(symbols[symbol] == 1) {
      running_total <- running_total*numbers[symbol+1]
    } else {
      running_total <- running_total+numbers[symbol+1]
    }
  }
  return(running_total)
}

next_possibility <- function(possibility,symbol_count) {
  curr_symbol <- copy(symbol_count)
  symbol_changed <- FALSE
  while(!symbol_changed) {
    if(possibility[curr_symbol] == 1) {
      possibility[curr_symbol] <- 0
      zeros_changed <- FALSE
      while(curr_symbol < symbol_count & !zeros_changed) {
        if(possibility[curr_symbol+1] == 0) {
          possibility[curr_symbol+1] <- 1
          curr_symbol <- curr_symbol+1
        } else {
          zeros_changed <- TRUE
        }
      }
      symbol_changed <- TRUE
    } else {
      curr_symbol <- curr_symbol-1
    }
  }
  return(possibility)
}


total <- 0
for(row in 1:nrow(input)) {
  row_total <- input[row]$X1
  valid_calibration <- FALSE
  nums <- as.numeric(strsplit(input[row]$X2,split = " ")[[1]])
  symbol_count <- length(nums)-1
  total_possibilites <- 2^symbol_count
  current_possibility <- rep(1,symbol_count)
  for(possibility in 1:total_possibilites) {
    possibility_total <- calculation_result(nums,current_possibility)
    if(possibility_total == row_total) {
      valid_calibration <- TRUE
      break
    } else if(possibility < total_possibilites) {
      current_possibility <- next_possibility(current_possibility,symbol_count)
    }
  }
  print(row_total)
  print(valid_calibration)
  if(valid_calibration) {
    total <- total + row_total
  }
}

sprintf("%.0f",total)

calculation_result2 <- function(numbers,symbols) {
  running_total <- numbers[1]
  for(symbol in 1:length(symbols)) {
    if(symbols[symbol] == 2) {
      running_total <- as.numeric(paste0(toString(bit64::as.integer64(running_total)),numbers[symbol+1]))
    } else if(symbols[symbol] == 1) {
      running_total <- running_total*numbers[symbol+1]
    } else {
      running_total <- running_total+numbers[symbol+1]
    }  }
  return(running_total)
}

next_possibility2 <- function(possibility,symbol_count) {
  curr_symbol <- copy(symbol_count)
  symbol_changed <- FALSE
  while(!symbol_changed) {
    if(possibility[curr_symbol] > 0) {
      possibility[curr_symbol] <- possibility[curr_symbol] -1
      zeros_changed <- FALSE
      while(curr_symbol < symbol_count & !zeros_changed) {
        if(possibility[curr_symbol+1] == 0) {
          possibility[curr_symbol+1] <- 2
          curr_symbol <- curr_symbol+1
        } else {
          zeros_changed <- TRUE
        }
      }
      symbol_changed <- TRUE
    } else {
      curr_symbol <- curr_symbol-1
    }
  }
  return(possibility)
}

total_pt2 <- 0
for(row in 1:nrow(input)) {
  row_total <- input[row]$X1
  valid_calibration <- FALSE
  nums <- as.numeric(strsplit(input[row]$X2,split = " ")[[1]])
  symbol_count <- length(nums)-1
  total_possibilites <- 3^symbol_count
  current_possibility <- rep(2,symbol_count)
  for(possibility in 1:total_possibilites) {
    possibility_total <- calculation_result2(nums,current_possibility)
    if(possibility_total == row_total) {
      valid_calibration <- TRUE
      break
    } else if(possibility < total_possibilites) {
      current_possibility <- next_possibility2(current_possibility,symbol_count)
    }
  }
  print(row_total)
  print(valid_calibration)
  if(valid_calibration) {
    total_pt2 <- total_pt2 + row_total
  }
}

sprintf("%.0f",total_pt2)
