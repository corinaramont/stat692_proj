library(dplyr)

source("functions/read_multiple_sheets.R") # load function

# load on macOS
path = "datasets/hai_only.xlsx"
data = ((multiplesheets(path))$table) 

# checking for variables that are empty
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

# remove empty variables
new = data[,-c(extra_ind)]

# summary stats
table(new$gender) # 710 female, 783 male, 496 unknown
table(new$country) # only 63 unknowns
table(new$age) # only 546 unknowns
table(new$disease)

