library(dplyr)
library(randomForest)
library(caret)
library(ggplot2)
library(tidyr)

### Setting up data ------------------------------------------------------------

data = read.csv("datasets/KMCC DM CRC.csv")

# set factor variables
var_ind = c(2,5,6,8,9,10,11,12,13,14,15,16)
for(i in 1:length(var_ind)){
  data[,var_ind[i]] = as.factor(data[,var_ind[i]])
}

new_data = data
miss_var_ind = c(11,12,14,15,16)
# if certain column values = 6 or 9, then replace the value with an NA
new_data[, miss_var_ind][new_data[,miss_var_ind] == 6 | new_data[,miss_var_ind] == 9] = NA

# remove observations with missing values
me_miss_ind = which(is.na(new_data$Moderate_Exer))
hx_miss_ind = which(is.na(new_data$hx_colorectal_polyps))
ed_miss_ind = which(is.na(new_data$education))
alc_miss_ind = which(is.na(new_data$alc_stat))
smk_miss_ind = which(is.na(new_data$smk_stat))
all_miss_ind = unique(c(me_miss_ind, hx_miss_ind, 
                        ed_miss_ind, alc_miss_ind, smk_miss_ind))
all_missing_data = new_data[all_miss_ind,]

# finalized dataset we are working with for modeling
remain_data = (new_data[-all_miss_ind,])[,-1]
remain_data$avg_age = (remain_data$FU_AGE + remain_data$E_AGE)/2
remain_data = remain_data[,-c(2,3)] # removes FU_AGE and E_AGE

remain0_data = remain_data %>% filter(CRC == 0)
remain1_data = remain_data %>% filter(CRC == 1)


s