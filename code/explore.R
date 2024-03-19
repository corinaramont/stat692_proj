library(dplyr)
library(randomForest)
library(caret)
library(ggplot2)

data = read.csv("datasets/KMCC DM CRC.csv")
var_ind = c(2,5,6,8,9,10,11,12,13,14,15,16)
for(i in 1:length(var_ind)){
  data[,var_ind[i]] = as.factor(data[,var_ind[i]])
}

# create copy of data and replace certain col values w/ NA
new_data = data
miss_var_ind = c(11,12,14,15,16)
# if certain column values = 6 or 9, then replace the value with an NA
new_data[, miss_var_ind][new_data[,miss_var_ind] == 6 | new_data[,miss_var_ind] == 9] = NA
# time on study
new_data$time_on_study = data$FU_AGE - data$E_AGE
clean_data = na.omit(new_data)

# check for missingness
ggplot_missing = function(x){
  if(!require(reshape2)){warning('you need to install reshape2')} 
  require(reshape2)
  require(ggplot2)
  #### This function produces a plot of the missing data pattern
  #### in x. It is a modified version of a function in the 'neato' package
  x %>%
    is.na %>%
    melt %>% ggplot(data = .,
                    aes(x = Var2,
                        y = Var1)) +
    geom_raster(aes(fill = value)) + scale_fill_grey(name = "",
                                                     labels = c("Present","Missing")) + theme_minimal() +
    theme(axis.text.x = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
} 
ggplot_missing(new_data) # Moderate_Exer and hx_colorectal_polyps have majority missing
sum(is.na(new_data$Moderate_Exer)) #num of missing Moderate_Exer values = 2436
sum(is.na(new_data$hx_colorectal_polyps)) # num of missing hx values = 2277
me_miss_ind = which(is.na(new_data$Moderate_Exer))
hx_miss_ind = which(is.na(new_data$hx_colorectal_polyps))
ed_miss_ind = which(is.na(new_data$education))
alc_miss_ind = which(is.na(new_data$alc_stat))
smk_miss_ind = which(is.na(new_data$smk_stat))
all_miss_ind = unique(c(me_miss_ind, hx_miss_ind, ed_miss_ind, alc_miss_ind, smk_miss_ind))
all_missing_data = new_data[all_miss_ind,]
remain_data = new_data[-all_miss_ind,]
# summary stats
not_new = new_data%>% filter(CRC == 0)
crc_new = new_data%>% filter(CRC == 1)
ggplot_missing(not_new)
ggplot_missing(crc_new)
par(cex.main=1)
boxplot(time_on_study~CRC,data=new_data,
        main="Duration enrolled in study",
        col = c(rgb(0.83, 0.62, 0.62), rgb(0.62, 0.69, 0.83)),
        xlab="CRC status", ylab="Time on study (years)")
# Add a legend
legend("topright", legend = c("non-CRC = 0 (n = 14381)","CRC = 1 (n = 189)") , 
       col = c(rgb(0.83, 0.62, 0.62), rgb(0.62, 0.69, 0.83)), 
       bty = "n", pch=20 , pt.cex = 3, cex = 0.85, 
       horiz = FALSE, inset = c(0.03, 0.1))

# random forest play time

# 1. set seed
set.seed(692)

# 2. partition data for training and testing
train_index = createDataPartition(remain_data$CRC, p = .8, list = FALSE) %>% 
  as.vector(.)
train_data = (remain_data[train_index,])[,-1]
test_data = (remain_data[-train_index,])[,-1]

rf = randomForest(CRC ~ ., data = train_data, proximity = T)

### Visualize variable importance ----------------------------------------------

# Get variable importance from the model fit
ImpData <- as.data.frame(importance(rf))
ImpData$Var.Names <- row.names(ImpData)

ggplot(ImpData, aes(x=Var.Names, y=MeanDecreaseGini)) +
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
