library(dplyr)
library(randomForest)
library(caret)

data = read.csv("datasets/KMCC DM CRC.csv")

# time on study
time_on_study = data$FU_AGE - data$E_AGE

# random forest play time


