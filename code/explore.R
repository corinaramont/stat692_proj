library(dplyr)

data = read.csv("datasets/KMCC DM CRC.csv")
time_on_study = data$FU_AGE - data$E_AGE


