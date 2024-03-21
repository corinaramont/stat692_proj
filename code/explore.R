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
new_data$time_on_study = data$FU_AGE - data$E_AGE

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

par(cex.main=1)
boxplot(time_on_study~CRC,data=new_data,
        main="Duration on study",
        col = c(rgb(0.83, 0.62, 0.62), rgb(0.62, 0.69, 0.83)),
        xlab="CRC status", ylab="Time on study (years)")
# Add a legend
legend(1.25, 20,legend = c("non-CRC = 0 (n = 14381)","CRC = 1 (n = 189)") , 
       col = c(rgb(0.83, 0.62, 0.62), rgb(0.62, 0.69, 0.83)), 
       bty = "n", pch=20 , pt.cex = 3, cex = 0.85, 
       horiz = FALSE, inset = c(0.03, 0.1))


