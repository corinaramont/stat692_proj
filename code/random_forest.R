library(dplyr)
library(randomForest)
library(caret)
library(ggplot2)

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
remain_data = new_data[-all_miss_ind,]

### RANDOM FOREST MODELING -----------------------------------------------------

# 1. set seed
set.seed(692)

# 2. partition data for training and testing
train_index = createDataPartition(remain_data$CRC, p = .8, list = FALSE) %>% 
  as.vector(.)
train_data = (remain_data[train_index,])[,-1]
test_data = (remain_data[-train_index,])[,-1]

rf = randomForest(CRC ~ ., data = train_data, proximity = T)

### Visualize variable importance ----------------------------------------------

# Get variable importance from the initial model fit using training data
imp_data = as.data.frame(importance(rf))
imp_data$Var.Names = row.names(imp_data)

ggplot(imp_data, aes(x=Var.Names, y=MeanDecreaseGini)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=MeanDecreaseGini), color="skyblue") +
  geom_point(aes(size = MeanDecreaseGini), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )