#Before 
install.packages('readr', dependencies = TRUE)
install.packages('ggfortify', dependencies = TRUE)
install.packages('forecast', dependencies = TRUE)
install.packages('zoo', dependencies = TRUE) 
library(readr)
library(ggfortify)
library(forecast)
library(zoo)
getwd() 
setwd('..//Downloads') #change working directory to your data file location
dir() 
#Before 

#Plot the Hong Kong's Industrial Production Index.
ip <- read_csv("HK_IP.csv") 
ip_ts <- ts(ip$IndustProd, frequency = 4, start = c(1982))
autoplot(ip_ts) + ggtitle("Manufacturing Industrial Production in Hong Kong in 1982Q1 - 2020Q1")

#b.	Use the following forecasting methods to do 1-step ahead forecast for the industrial production index in 2020Q2.
#i.	Naïve forecast
#ii.	Seasonal naïve forecast
#iii.	The drift model
#iv.	2-quarter-moving-average
#v.	4-quarter-moving-average

naive(ip_ts, 1) #simple naive forecast or use rwf(ip_ts, 1)
snaive(ip_ts, 1) #seasonal naive forecast
rwf(ip_ts, 1, drift = TRUE) #drift model
ma2 = rollmean(ip_ts, k = 2, fill = NA, align = "right") #2-quarter-moving-average
tail(ma2, 1)
ma4 = rollmean(ip_ts, k = 4, fill = NA, align = "right") #4-quarter-moving-average
tail(ma4, 1)
#b. 

#f. (9 points, 3 points each) Split the data into the training sample (1982Q1-2018Q4) and test sample (2019Q1-2020Q1) and report in a table the forecasting performance (RMSE, MAPE, and Theil's U) of the following forecasting methods (Hints: you can use the "accuracy" command in R):
#  i. Naïve forecast
#ii. Seasonal naïve forecast
#iii. The drift model

ip_train_sample <- window(ip_ts, end = c(2018,4))
ip_test_sample <- window(ip_ts, start = c(2019,1), end = c(2020,1))
length(ip_test_sample) #5-step ahead forecast
ipfit1 <- rwf(ip_train_sample, h = 5) #simple naive forecast
ipfit2 <- snaive(ip_train_sample, h = 5) #seasonal naive forecast
ipfit3 <- rwf(ip_train_sample, h = 5, drift = TRUE) #drift model
#report forecasting performance measures (RMSE, MAPE and Theil's U)
accuracy(ipfit1, ip_test_sample)
accuracy(ipfit2, ip_test_sample)
accuracy(ipfit3, ip_test_sample)
#f


#2a. Decompose the data into the seasonal, trend-cycle and remainder component. Report the graph for the three components
stlfit <- stl(ip_ts, t.window = NULL, s.window = "periodic", robust = TRUE)
#seasonal, trend, and remainder components are stored in stlfit$timeseries 
# show the first and last few lines of the component
head(stlfit$time.series,8) 
tail(stlfit$time.series,8) 
autoplot(stlfit) + ggtitle("Seasonal, Trend, and Remainder components of Hong Kong Industrial Production Index")
##2a

#2b forecast the industrial production from 2020Q2 to 2020Q4
stlforecast <- stlf(ip_ts, method = 'naive', h = 3) #3-step ahead
summary(stlforecast)
# stlforecast$mean #alternatively retrieve the point forecast 
autoplot(stlforecast) + ylab("New Industrial Production Index")

