#Load necessary libraries
install.packages("tseries",dependencies = TRUE)
library(forecast)
library(tseries)
#Load in built R data
f1=read.csv("C:\\Users\\Raghavendra Reddy\\Documents\\SENSEX.csv")

#Show the data
f1<-ts(f1, start=c(2016,1), end=c(2018,9), frequency = 12)
plot(f1)

#Check type of the data
abline(reg = lm(f1-time(f1)))
plot(decompose(f))
d <- decompose(f1)
plot(d$trend)
plot(decompose(f1, type = 'multiplicative'))
decompose(f1,type = 'seasonality')
d$seasonal
plot(d$seasonal)
d$trend
plot(d$trend)
d$random
plot(d$random)
############################################
#Holt Winter
#Load necessary libraries

library(TTR)
#Holt Winters Exponential Smotthing without trend and without seasonal
#Single Exponenetial Smoothing
forecast1 <- HoltWinters(f1, beta= FALSE, gamma = FALSE)
#sHOW FITTED VALUES
forecast1$fitted
#Plot forecast values
plot(forecast1, main = 'Holt Winters Exponential Smoothing without trend and without seasonal component')
#Forecats for next 10 time periods)
library(forecast)
forecast2 <- forecast(forecast1, h=10)
forecast2
forecast2$model
plot(forecast2)
#Plot the residuals
plot(forecast2$residuals)
#MAE in the prdiction
mae = mean(abs(forecast2$residuals),na.rm = TRUE)
mae
###########################################
#Holt Winters Exponential Smotthing with ADDITIVE SEOSANALITY
#Doublge Exponential Smoothing
forecast3 <- HoltWinters(f1, seasonal ='additive')
#sHOW FITTED VALUES
forecast3$fitted
#Plot forecast values
plot(forecast3, main = 'Holt Winters Exponential Smoothing with additive seasonality')
#Forecats for next 10 time periods)
forecast4 <- forecast(forecast3, h=10)
forecast4
forecast4$model
plot(forecast4)
#Plot the residuals
plot(forecast4$residuals)
#MAE in the prdiction
mae = mean(abs(forecast4$residuals),na.rm = TRUE)
mae
##################################
#Holt Winters Exponential Smotthing with Multiplicative SEOSANALITY
#Triple Exponential Smoothing
forecast5 <- HoltWinters(f1, seasonal ='multiplicative')
#sHOW FITTED VALUES
forecast5$fitted
#Plot forecast values
plot(forecast5, main = 'Holt Winters Exponential Smoothing with multiplicative seasonality')
#Forecats for next 10 time periods)
forecast6 <- forecast(forecast5, h=10)
forecast6
forecast6$model
plot(forecast6)
#Plot the residuals
plot(forecast6$residuals)
#MAE in the prdiction
mae = mean(abs(forecast6$residuals),na.rm = TRUE)
mae
