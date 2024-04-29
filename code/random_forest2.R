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

remain0_data = remain_data %>% filter(CRC== 0)
remain1_data = remain_data %>% filter(CRC == 1)

### RANDOM FOREST MODELING with training data ----------------------------------

## 1. set seed
set.seed(692)

## 2. partition data for training and testing
train0_index = createDataPartition(remain0_data$CRC, p = .8, list = FALSE) %>% 
  as.vector(.)
train1_index = createDataPartition(remain1_data$CRC, p = 0.67, list = FALSE) %>%
  as.vector(.)
train0 = remain0_data[train0_index,]
train1 = remain1_data[train1_index,]
test0 = remain0_data[-train0_index,]
test1 = remain1_data[-train1_index,]
train_data = rbind(train0, train1)
test_data = rbind(test0, test1)

## 3. run model
#rf1 = randomForest(CRC ~ ., data = train_data, ntree = 10000, 
#                   sampsize = c('0' = 30, '1' = 30))
#rf2 = randomForest(CRC ~ ., data = train_data, ntree = 10000, 
#                   sampsize = c('0' = 40, '1' = 40))
#rf3 = randomForest(CRC ~ ., data = train_data, ntree = 10000, 
#                   sampsize = c('0' = 50, '1' = 50))
#rf4 = randomForest(CRC ~ ., data = train_data, ntree = 10000, 
#                   sampsize = c('0' = 60, '1' = 60))
#rf5 =randomForest(CRC ~ ., data = train_data, ntree = 10000, 
#                  sampsize = c('0' = 70, '1' = 70))
#rf6 = randomForest(CRC ~ ., data = train_data, ntree = 10000, 
#                   sampsize = c('0' = 80, '1' = 80))
rf7 = randomForest(CRC ~ ., data = train_data, ntree = 10000, 
                   sampsize = c('0' = 90, '1' = 90))
print(rf)


### TUNING RANDOM FOREST -------------------------------------------------------

## 1. tune number of trees built aka ntree

# plot with all 3 types of errors
rf = rf7 
oob_data = data.frame(
  trees = rep(1:nrow(rf$err.rate), 3), 
  type = rep(c("OOB","No history of CRC","History or currently has CRC"), 
             each = nrow(rf$err.rate)),
  error = c(rf$err.rate[,"OOB"], rf$err.rate[,"0"], 
            rf$err.rate[,"1"]))
ggplot(data = oob_data, aes(x = trees, y= error)) + 
  geom_line(aes(color = type))

# plot with only OOB
plot((oob_data %>% filter(type == "OOB"))$trees,
     (oob_data %>% filter(type == "OOB"))$error, type = "l", 
     xlab = "Number of trees built",
     ylab = "OOB error",
     main = "OOB error for the number of trees built")
abline(v = 4000,col = rgb(0.49, 0.81, 0.54))


## 2. tune number of variables selected at each split aka mtry
tuneRF(train_data[,-1], train_data$CRC, nTreeTry = 4000, 
       stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE,
       sampsize = c('0' = 90, '1' = 90))


### RERUN RANDOM FOREST USING TUNED PARAMETERS ---------------------------------
rf_final = randomForest(CRC ~ ., data = train_data, ntree = 4000, mtry = 6, 
                        sampsize = c('0' = 90, '1' = 90))
print(rf_final)


# visualizing variable importance
# Get variable importance from the initial model fit using training data
imp_data = as.data.frame(importance(rf_final))
imp_data$Var.Names = row.names(imp_data)

# the higher the mean decrease gini = the higher the variable importance
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
  )+xlab("Variable Names") + ylab("Mean Decrease Gini")+ggtitle("Importance of Variables in Model")


### USING RANDOM FOREST MODELING ON TESTING DATA -------------------------------

rf_pred = predict(rf_final, test_data)
confusionMatrix(rf_pred, test_data$CRC)
