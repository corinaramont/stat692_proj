library(dplyr)
library(caret)
library(ggplot2)

### Set up data ----------------------------------------------------------------
data = read.csv("datasets/KMCC DM CRC.csv")

# setting factor variables
var_ind = c(2,5,6,8,9,10,11,12,13,14,15,16)
for(i in 1:length(var_ind)){
  data[,var_ind[i]] = as.factor(data[,var_ind[i]])
}
new_data = data
miss_var_ind = c(11,12,14,15,16)
# if certain column values = 6 or 9, then replace the value with an NA
new_data[, miss_var_ind][new_data[,miss_var_ind] == 6 | 
                           new_data[,miss_var_ind] == 9] = NA
# time on study
data$avg_age = (data$FU_AGE + data$E_AGE)/2

# figures
par(cex.main=1)
boxplot(avg_age~CRC,data=data,
        main="Average age during the study",
        col = c(rgb(0.83, 0.62, 0.62), rgb(0.62, 0.69, 0.83)),
        xlab="CRC status", ylab="Age in years")
# Add a legend
legend(1.75,100,legend = c("non-CRC = 0 (n = 14381)","CRC = 1 (n = 189)") , 
       col = c(rgb(0.83, 0.62, 0.62), rgb(0.62, 0.69, 0.83)), 
       bty = "n", pch=20 , pt.cex = 3, cex = 0.85, 
       horiz = FALSE, inset = c(0.03, 0.1))

boxplot(glu~CRC,data=data,
        main="Fasting glucose levels in patients",
        col = c(rgb(0.83, 0.62, 0.62), rgb(0.62, 0.69, 0.83)),
        xlab="CRC status", ylab="Glucose level (mg/dl)")
# Add a legend
legend("topright",legend = c("non-CRC = 0 (n = 14381)","CRC = 1 (n = 189)") , 
       col = c(rgb(0.83, 0.62, 0.62), rgb(0.62, 0.69, 0.83)), 
       bty = "n", pch=20 , pt.cex = 3, cex = 0.85, 
       horiz = FALSE, inset = c(0.03, 0.1))

### Checking for missingness ---------------------------------------------------

ggplot_missing = function(x){
  if(!require(reshape2)){warning('you need to install reshape2')} 
  require(reshape2)
  require(ggplot2)
  # This function produces a plot of the missing data pattern
  # in x. It is a modified version of a function in the 'neato' package
  x %>%
    is.na %>%
    melt %>% ggplot(data = .,
                    aes(x = Var2,
                        y = Var1)) +
    geom_raster(aes(fill = value)) + 
    scale_fill_grey(name = "", labels = c("Present","Missing")) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
} 
ggplot_missing(new_data) # Moderate_Exer and hx_colorectal_polyps have majority missing
sum(is.na(new_data$Moderate_Exer)) #num of missing Moderate_Exer values = 2436
sum(is.na(new_data$hx_colorectal_polyps)) # num of missing hx values = 2277

### Summary stats --------------------------------------------------------------

not_new = new_data%>% filter(CRC == 0)
crc_new = new_data%>% filter(CRC == 1)

ggplot_missing(not_new)
ggplot_missing(crc_new)

### Graphs and figures of our analyzed data ------------------------------------

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
remain0_data = remain_data %>% filter(CRC == 0)
remain1_data = remain_data %>% filter(CRC == 1)
cor(remain_data[,c(2,3,6)])
