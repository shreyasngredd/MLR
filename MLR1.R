#Prepare a prediction model for profit of 50_startups data.
#Do transformations for getting better predictions of profit and
#make a table containing R^2 value for each prepared model.

##Load and Attach the data
fifty<- read.csv(file.choose())
View(fifty)
attach(fifty)

##Creating dummy variables for State
fifty <- cbind(fifty,ifelse(fifty$State=='New York',1,0), 
               ifelse(fifty$State=='California',1,0),  
               ifelse(fifty$State=='Florida',1,0))

#renaming columns
colnames(fifty)[6] <- "New.York"
colnames(fifty)[7] <- "California"
colnames(fifty)[8] <- "Florida"

#Removing State column
fifty$State<- NULL

attach(fifty)

##summarize the data
summary(fifty)

#R.D.Spend: Mean= 73722, Median= 73051; As Mean>Median,it is skewed to the left
#Administartion: Mean= 121345, Median= 122700; As Median>Mean, it is skewed to the right
#Marketing.Spend: Mean= 211025, Median= 212716; As Median>Mean, it is skewed to the right
#Profit: Mean= 112013, Median= 107978; As Median>Mean, it is skewed to the right
#New.York: Mean= 0.34, Median= 0.00; As Mean>Median,it is skewed to the left
#California: Mean= 0.34, Median= 0.00; As Mean>Median,it is skewed to the left
#Florida: Mean= 0.32, Median= 0.00; As Mean>Median,it is skewed to the left

library(DataExplorer)
plot_str(fifty)
str(fifty)

plot_missing(fifty)

##Data Visualization- Histogram
plot_histogram(R.D.Spend)
plot_histogram(Administration)
plot_histogram(Marketing.Spend)
plot_histogram(Profit)
plot_histogram(New.York)
plot_histogram(California)
plot_histogram(Florida)

##Data Visualization- Plot Density
plot_density(R.D.Spend)
plot_density(Administration)
plot_density(Marketing.Spend)
plot_density(Profit)
plot_density(New.York)
plot_density(California)
plot_density(Florida)

##Correlation Scatterplot
pairs(fifty)
#There is no linearity with states, hence, we are excluding it.

#Correlation Coefficient matrix
cor(fifty)

plot(fifty)
#Deducing from the scatter correlation, the states will be avoided
library(dplyr)
fift<- select(fifty,-New.York,-California,-Florida)
View(fift)

##Partial Correlation matrix
library(corpcor)
cor2pcor(cor(fift))

###The Linear Model of interest-1
model.fif<- lm(Profit~ R.D.Spend+Administration+Marketing.Spend,data=fift)
summary(model.fif) 
#Administration,Marketing Spend and the States are insignificant

model.admin<- lm(Profit~Administration,data=fift)
summary(model.admin)
#Administration is insignificant

model.MS<- lm(Profit~Marketing.Spend,data=fift)
summary(model.MS)
#Marketing.Spend is significant

## Deletion Diagnostics for identifying influential observations
library(car)
influenceIndexPlot(model.fif, id.n=3)
influencePlot(model.fif, id.n=3)

model.fif2<- lm(Profit~R.D.Spend+Administration+Marketing.Spend, data=fift[-47, ])
summary(model.fif2)

model.fif3<- lm(Profit~R.D.Spend+Administration+Marketing.Spend, data=fift[-c(47,49), ])
summary(model.fif3)

model.fif4<- lm(Profit~R.D.Spend+Administration+Marketing.Spend, data=fift[-c(46,47,49,50), ])
summary(model.fif4)

###Variance Inflation factor
vif(model.fif)
###Added Variable plot
avPlots(model.fif,id.n=2,id.cex=0.7)
#On the basis of vif and avPlots, Administration variable is deleted

###Final Model
model.final<- lm(Profit~R.D.Spend+Marketing.Spend,data=fift)
summary(model.final)

vif(model.final)

avPlots(model.final,id.n=2,id.cex=0.7)

plot(model.final)

qqPlot(model.final,id.n = 5)

summary(model.final$residuals)#to check normal distribution

##RMSE
sqrt(mean(model.final$residuals^2))
