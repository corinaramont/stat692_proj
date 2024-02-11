library(dplyr)

source("functions/read_multiple_sheets.R") # load function

# load on macOS
path = "datasets/hai_only.xlsx"
data = ((multiplesheets(path))$table) 

extra_vars = c()
extra_ind = c()
for(i in 1:ncol(data)){
  temp = data[,i]
  len = length(unique(temp))
  if(len < 2){
    extra_vars = c(extra_vars, colnames(data)[i])
    extra_ind = c(extra_ind, i)
  }
}

data %>% select(extra_vars[2])
