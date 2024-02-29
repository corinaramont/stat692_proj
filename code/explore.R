library(dplyr)
library(randomForest)
library(caret)

data = read.csv("datasets/KMCC DM CRC.csv")
var_ind = c(2,5,6,8,9,10,11,12,13,14,15,16)
for(i in 1:length(var_ind)){
  data[,var_ind[i]] = as.factor(data[,var_ind[i]])
}

# create copy of data and replace certain col values w/ NA
new_data = data
miss_var_ind = c(11,12,14,15,16)
srt    = proc.time()[3]
# if certain column values = 6 or 9, then replace the value with an NA
new_data[, miss_var_ind][new_data[,miss_var_ind] == 6 | new_data[,miss_var_ind] == 9] = NA
end = proc.time()[3]
time = end - srt
print(time)
# time on study
time_on_study = data$FU_AGE - data$E_AGE

# summary stats
crc_new = new_data%>% filter(CRC == 0)
not_new = new_data%>% filter(CRC == 1)

# random forest play time

# 1. set seed
set.seed(692)

# 2. partition data for training and testing
train_index = createDataPartition(data$CRC, p = .8, list = FALSE) %>% 
  as.vector(.)
train_data = (data[train_index,])[,-1]
test_data = (data[-train_index,])[,-1]

rf = randomForest(CRC ~ ., data = train_data, proximity = T)
