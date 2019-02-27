#####MULTI LINEAR REGRESSION#####

#Consider only the below columns and prepare a prediction model for predicting Price.
#Corolla<-Corolla[c("Price","Age_08_04","KM","HP","cc","Doors",
#"Gears","Quarterly_Tax","Weight")] 

#Load and Attach the data
corrolla<- read.csv(file.choose())
View(corrolla)
corr<- corrolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(corr)
attach(corr)

#summarize data
summary(corr)

#Price: Mean= 10731, Median= 9900; As Mean>Median,it is skewed to the left
#Age_08_04: Mean= 55.95, Median= 61.00; As Mean<Median,it is skewed to the right
#KM: Mean= 68533, Median= 63390; As Mean>Median,it is skewed to the left
#HP: Mean= 101.5, Median= 110.0; As Mean<Median,it is skewed to the right
#cc: Mean= 1577, Median= 1600; As Mean<Median,it is skewed to the right
#Doors: Mean= 4.033, Median= 4.000; As Mean>Median,it is skewed to the left
#Gears: Mean= 5.026, Median= 5.000; As Mean>Median,it is skewed to the left
#Quarterly_Tax: Mean= 87.12, Median= 85.00; As Mean>Median,it is skewed to the left
#Weight: Mean= 1072, Median= 1070; As Mean>Median,it is skewed to the left

library(DataExplorer)
plot_str(corr)
str(corr)

plot_missing(corr)

##Data Visualization- Histogram
plot_histogram(Price)
plot_histogram(Age_08_04)
plot_histogram(KM)
plot_histogram(HP)
plot_histogram(cc)
plot_histogram(Doors)
plot_histogram(Gears)
plot_histogram(Quarterly_Tax)
plot_histogram(Weight)

##Data Visualization- Plot Density
plot_density(Price)
plot_density(Age_08_04)
plot_density(KM)
plot_density(HP)
plot_density(cc)
plot_density(Doors)
plot_density(Gears)
plot_density(Quarterly_Tax)
plot_density(Weight)

##Correlation between X and Y
pairs(corr)

##Correlation Coefficient matrix
cor(corr)

##Partial Correlation matrix
library(corpcor)
cor2pcor(cor(corr))

###The Linear Model of interest-1
model.corr<- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight)
summary(model.corr) 
#cc and Doors are insignificant

model.cc<- lm(Price~cc)
summary(model.cc)
#cc is significant

model.d<- lm(Price~Doors)
summary(model.d)
#Doors is significant

model.cd<- lm(Price~Doors+cc)
summary(model.cd)
#Doors and cc are significant

## Deletion Diagnostics for identifying influential observations
library(car)
influenceIndexPlot(model.corr, id.n=3)
influencePlot(model.corr, id.n=3)


model.corr1<- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data=corr[-81, ])
summary(model.corr1) 
#cc is significant; Doors is insignificant

model.corr2<- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data=corr[-c(81,222,961), ])
summary(model.corr2)
#R^2 value improved and all values are signficant. 

#Hence, we will take model.corr2 as the final model.

###Variance Inflation factor
vif(model.corr2)

avPlots(model.corr2,id.n=2,id.cex=0.7)

plot(model.corr2)

qqPlot(model.corr2,id.n = 5)

summary(model.corr2$residuals)#to check normal distribution

##RMSE
sqrt(mean(model.corr2$residuals^2))
