library(data.table)
library(readr)
input <- data.table(read_delim("~/Documents/advent of code/advent of code 2024/input_day5_sample.txt",col_names = FALSE,delim = "thiswillneverappear",skip_empty_rows = TRUE))

rules <- input[X1 %like% "\\|"]
print_orders <- input[X1 %like% "\\,"]

sum_middles <- 0
sum_middles_misordered <- 0

for (print_order in 1:nrow(print_orders)) {
  correct_order <- TRUE
  order_corrected <- FALSE
  print_order_split <- strsplit(print_orders[print_order]$X1,split = ",")[[1]]
  rules$X2 <- 0
  for(page in 1:length(print_order_split)) {
    rules_apply <- rules$X1 %like% print_order_split[page]
    rules$X2 <- rules$X2 + rules_apply
  }
  rules_apply <- rules[X2 == 2]
  while(order_corrected == FALSE) {
    order_corrected <- TRUE
    for(rule in 1:nrow(rules_apply)) {
      rule_order <- strsplit(rules_apply[rule]$X1,split = "\\|")[[1]]
      if(grep(rule_order[1],print_order_split) > grep(rule_order[2],print_order_split)) {
        correct_order <- FALSE
        order_corrected <- FALSE
        ### steps to attempt to correct
        location1 <- grep(rule_order[1],print_order_split)
        location2 <- grep(rule_order[2],print_order_split)
        print_order_split <- replace(print_order_split,c(location1,location2),print_order_split[c(location2,location1)])
      }
    }
    order_corrected <- ifelse(correct_order,TRUE,order_corrected)
  }
  if(correct_order) {
  ## find middle
    middle <- as.numeric(print_order_split[ceiling(length(print_order_split)/2)])
    sum_middles <- sum_middles + middle
  } else {
    middle <- as.numeric(print_order_split[ceiling(length(print_order_split)/2)])
    sum_middles_misordered <- sum_middles_misordered + middle
  }
}

print(sum_middles)
print(sum_middles_misordered)