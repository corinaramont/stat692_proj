library(dplyr)

source("functions/read_multiple_sheets.R") # load function

# load on macOS
path = "datasets/class_only_test.xlsx"
data = ((multiplesheets(path))$table) 

extra_vars = c()
for(i in 1:ncol(data)){
  temp = data[,i]
  len = length(unique(temp))
  if(len < 2){
    extra_vars = c(extra_vars, colnames(data)[i])
  }
}

data %>% select(extra_vars[2])
