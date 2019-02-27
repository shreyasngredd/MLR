######MULTI LINEAR REGRESSION######

#Predict sales of the computer (Using price as output)

##Load and Attach the data
comp<- read.csv(file.choose())
View(comp)

library(dplyr)
compu<- select(comp,-X)
View(compu)
attach(compu)

compu <- cbind(compu,ifelse(compu$cd=='yes',1,0),
               ifelse(compu$multi=='yes',1,0),
               ifelse(compu$premium=='yes',1,0))

#renaming columns
colnames(compu)[11] <- "cd_num"
colnames(compu)[12] <- "multi_num"
colnames(compu)[13] <- "premium_num"

#removing columns cd, multi,premium
compu$cd<- NULL
compu$multi<- NULL
compu$premium<- NULL

attach(compu)
##summarize the data
summary(compu)

#price: Mean= 2220, Median= 2144; As Mean>Median,it is skewed to the left
#speed: Mean= 52.01, Median= 50.00; As Mean>Median, it is skewed to the left
#hd: Mean= 416.6, Median= 340.0; As Mean>Median, it is skewed to the left
#ram: Mean= 8.287 , Median= 8.000; As Median>Mean, it is skewed to the left
#screen: Mean= 14.61 , Median= 14.00; As Mean>Median,it is skewed to the left
#ads: Mean= 221.3, Median= 246; As Median>Mean, it is skewed to the right
#trend: Mean= 15.93, Median= 16.00; As Median>Mean, it is skewed to the right
#cd_num: Mean= 0.4646, Median= 0.00; As Mean>Median,it is skewed to the left
#multi_num: Mean= 0.1395, Median= 0.000; As Mean>Median,it is skewed to the left
#premimum_num: Mean= 0.9022, Median= 1.000; As Median>Mean, it is skewed to the right

library(DataExplorer)
plot_str(compu)
str(compu)

plot_missing(compu)

##Data Visualization- Histogram
plot_histogram(price)
plot_histogram(speed)
plot_histogram(hd)
plot_histogram(ram)
plot_histogram(screen)
plot_histogram(ads)
plot_histogram(trend)
plot_histogram(cd_num)
plot_histogram(multi_num)
plot_histogram(premium_num)

##Data Visualization- Plot Density
plot_density(price)
plot_density(speed)
plot_density(hd)
plot_density(ram)
plot_density(screen)
plot_density(ads)
plot_density(trend)
plot_density(cd_num)
plot_density(multi_num)
plot_density(premium_num)

##Correlation between X and Y
pairs(compu)

##Correlation Coefficient matrix
cor(compu)

##Partial Correlation matrix
library(corpcor)
cor2pcor(cor(compu))

###The Linear Model of interest-1
model.compu<- lm(price~ speed+hd+ram+screen+ads+trend+cd_num+multi_num+premium_num)
summary(model.compu)
#all the values are significant, The R^2 value can be improved.

## Deletion Diagnostics for identifying influential observations
library(car)
influenceIndexPlot(model.compu, id.n=3)
influencePlot(model.compu, id.n=3)

model.compu1<- lm(price~speed+hd+ram+screen+ads+trend+cd_num+multi_num+premium_num, data=compu[-3784, ])
summary(model.compu1) 
#R^2 has slightly decreased

model.compu2<- lm(price~speed+hd+ram+screen+ads+trend+cd_num+multi_num+premium_num, data=compu[-c(3784,4478), ])
summary(model.compu2) 
#R^2 has slightly decreased

model.compu3<- lm(price~speed+hd+ram+screen+ads+trend+cd_num+multi_num+premium_num, data=compu[-c(1441,1701), ])
summary(model.compu3) 
#R^2 has improved slightly. 

model.compu4<- lm(price~speed+hd+ram+screen+ads+trend+cd_num+multi_num+premium_num, data=compu[-c(1441,1701,3784,4478), ])
summary(model.compu4) 
#R^2 has decreased compared to model.compu3 
#Hence, we will take model.compu3 as the final model.

###Variance Inflation factor
vif(model.compu3)

avPlots(model.compu3,id.n=2,id.cex=0.7)

plot(model.compu3)

qqPlot(model.compu3,id.n = 5)

summary(model.compu3$residuals)#to check normal distribution

##RMSE
sqrt(mean(model.compu3$residuals^2))
