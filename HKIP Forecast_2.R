ip <- read_csv("HK_IP.csv") #import the data file
ip_ts <- ts(ip$IPGMFNQ, frequency = 4, start = c(1982, 1)) #turn index time series (IPGMFNQ) into
ts object
plot(ip_ts, main = "Hong Kong Manufacturing Industrial Production in 1982Q1-2020Q1")
ggtsdisplay(ip_ts) #detect seasonality and trend using ACF display

ip_in <- window(ip_ts, end = c(2018, 3)) #in-sample (training) data
# 2018Q4 to 2020Q3: 8-step ahead forecast
hwfit <- hw(ip_in, h = 8, seasonal = "multiplicative")
summary(hwfit)

ip_forecast <- ts(c(hwfit$fitted, hwfit$mean), frequency = 4, start = c(1982,1))
#plot the actual and predicted
autoplot(ip_ts, series = "Actual") +
  autolayer(ip_forecast, series = "Predicted") +
  ggtitle("Forecasts from theHolt-Winters' multiplicative method") + xlab("Year") +
  ylab("Manufacturing Industrial Production") +
  guides(colour=guide_legend(title="Forecast"))

##########  Calculate RMSE and MAPE of hold-out (test sample) ##########
ip_hold_out <- window(ip_ts, start = c(2018, 4),
                      end = c(2020, 1)) #actual values for hold-out-sample
forecast_hold_out <- window(hwfit$mean, start = c(2018, 4),
                            end = c(2020, 1)) #forecast values for hold-out-sample
res_hold_out <- ip_hold_out - forecast_hold_out
RMSE_hold_out <- sqrt(mean(res_hold_out^2))
MAPE_hold_out <- mean(abs(res_hold_out / ip_hold_out))
RMSE_hold_out; MAPE_hold_out*100 #output RMSE and MAPE for hold-out-sample period

# STL forecast from 2018Q4 to 2020Q3 similar to exponential smoothing
stlforecast <- stlf(ip_in, method = 'naive', h = 8) #8-step ahead forecast
#calculate RMSE and MAPE of hold-out sample of STL method
stlforecast_hold_out <- window(stlforecast$mean, start = c(2018, 4),
                               end = c(2020, 1)) #forecast values of STL method for hold-out-sample
res_hold_out_stl <- ip_hold_out - stlforecast_hold_out
RMSE_hold_out_stl <- sqrt(mean(res_hold_out_stl^2))
MAPE_hold_out_stl <- mean(abs(res_hold_out_stl / ip_hold_out))
RMSE_hold_out_stl; MAPE_hold_out_stl*100 #output RMSE and MAPE for hold-out-sample
period