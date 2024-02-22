library(dplyr)
library(randomForest)
library(caret)

data = read.csv("datasets/KMCC DM CRC.csv")

# time on study
time_on_study = data$FU_AGE - data$E_AGE

# random forest play time

# 1. set seed
set.seed(692)

# 2. partition data for training and testing
train_index = createDataPartition(data$CRC, p = .8, list = FALSE) %>% 
  as.vector(.)
train_data = data[train_index,]
test_data = data[-train_index,]
