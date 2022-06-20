install.packages('forecast')
install.packages('readr')
install.packages('zoo')
install.packages('gridExtra')
install.packages('ggfortify')

library(forecast)
library(readr)
library(zoo)
library(gridExtra)
library(ggfortify)
getwd()
setwd('..//Downloads') #change working directory to your data file location
dir()

ds_gdp <- read_csv('real_gdp_hk_chained2018.csv') #read the data file
gdp_ts <- ts(ds_gdp$real_GDP_chained2018, frequency = 4, start = c(1973,1)) #turn GDP series into ts object
## Construct the percentage year-to-year growth rate series grth_ts
grth_ts <- diff(gdp_ts, 4)/lag(gdp_ts, -4) * 100
head(grth_ts); tail(grth_ts) #show the first and last few lines 

## Plot GDP and GDP year-to-year growth rate
plot(gdp_ts, main = "Hong Kong GDP")
plot(grth_ts, main = "Hong Kong GDP Year-to-Year Growth Rate")

grth_ts_In <- window(grth_ts, end = c(2018, 4)) # In-sample period '1973-01-01~2018-12-31' 
grth_ts_hold <- window(grth_ts, start = c(2019, 1)) # Hold-out sample period
#head(grth_ts_In); tail(grth_ts_In)

p1 <- ggAcf(grth_ts_In)
p2 <- ggPacf(grth_ts_In)
gridExtra::grid.arrange(p1,p2,ncol=1) #plot ACF and PACF on the sample graph page

AIC_List <- vector('double', 0)
BIC_List <- vector('double', 0) #two lists for storing the AIC & BIC values
# calculate AIC & BIC with p from 0 to 6
i = 0 
# do the loop
for (p in 0:6){
  for (q in 0:6){
    i = i + 1 
    tmp <- Arima(grth_ts_In, order = c(p, 0, q)) #ARIMA model estimation
    print(paste0('This is iteration :', i, '; p = ', p, ' & q = ', q, '; AIC: ', 
                 tmp$aic, ' BIC: ', tmp$bic)) # retrieve AIC & BIC values
    BIC_List <- c(BIC_List, tmp$bic) 
    AIC_List <- c(AIC_List, tmp$aic) #append AIC &BIC values in the lists
    # print(tmp)
  }
}
which.min(AIC_List) 
which.min(BIC_List)

###
auto.arima(grth_ts_In, stepwise = FALSE, approximation = FALSE, seasonal = FALSE)

### Consider the model with lowest AIC (model 3a-AIC)
chosen_AIC <- Arima(grth_ts_In, order = c(5, 0, 5)) 
#summary(chosen_AIC)
res_chosen_AIC <- chosen_AIC$residuals
# Plot the ACF and PACF of the residuals 
p1_AIC <- ggAcf(res_chosen_AIC)
p2_AIC <- ggPacf(res_chosen_AIC)
gridExtra::grid.arrange(p1_AIC, p2_AIC, ncol=1)
# Ljung-Box test 
checkresiduals(chosen_AIC, lag = 4*4)

### Consider the model chosen by auto.arima (model 3b)Alternative
chosen_auto <- Arima(grth_ts_In, order = c(4, 1, 1))
res_chosen_auto <- chosen_auto$residuals
p1_auto <- ggAcf(res_chosen_auto)
p2_auto <- ggPacf(res_chosen_auto)
gridExtra::grid.arrange(p1_auto, p2_auto, ncol=1)
checkresiduals(chosen_auto, lag = 4*4)

### Consider the model with lowest BIC (model 3a-BIC)
chosen_BIC <- Arima(grth_ts_In, order = c(4, 0, 1))
#summary(chosen_BIC)
res_chosen_BIC <- chosen_BIC$residuals
p1_BIC <- ggAcf(res_chosen_BIC)
p2_BIC <- ggPacf(res_chosen_BIC)
gridExtra::grid.arrange(p1_BIC, p2_BIC, ncol=1)
checkresiduals(chosen_BIC, lag = 4*4)

### Consider the model chosen by auto.arima (model 3b)Alternative
chosen_auto <- Arima(grth_ts_In, order = c(4, 1, 1))
res_chosen_auto <- chosen_auto$residuals
p1_auto <- ggAcf(res_chosen_auto)
p2_auto <- ggPacf(res_chosen_auto)
gridExtra::grid.arrange(p1_auto, p2_auto, ncol=1)
checkresiduals(chosen_auto, lag = 4*4)

### Consider the model with lowest AIC (model 3a-AIC)
# produce the forecasts for the hold-out-sample period
f_chosen_AIC <- forecast(chosen_AIC, h = 6) #2019Q1-2020Q2: h = 6 steps ahead
# attach the values as well as the graph
point_f_chosen_AIC <- f_chosen_AIC$mean
point_f_chosen_AIC
plot(f_chosen_AIC)
# calculate RMSE and MAPE
residuals_hold_AIC <- point_f_chosen_AIC - grth_ts_hold
rmse_hold_AIC <- sqrt(sum(residuals_hold_AIC^2)/length(residuals_hold_AIC))
mape_hold_AIC <- sum(abs(residuals_hold_AIC/grth_ts_hold))/length(residuals_hold_AIC) 
rmse_hold_AIC; mape_hold_AIC

### Consider the model with lowest BIC (model 3a-BIC)
# produce the forecasts for the hold-out-sample period
f_chosen_BIC <- forecast(chosen_BIC, h = 6)
# attach the values as well as the graph
point_f_chosen_BIC <- f_chosen_BIC$mean
point_f_chosen_BIC
plot(f_chosen_BIC)
# calculate RMSE and MAPE
residuals_hold_BIC <- point_f_chosen_BIC - grth_ts_hold
rmse_hold_BIC <- sqrt(sum(residuals_hold_BIC^2)/length(residuals_hold_BIC))
mape_hold_BIC <- sum(abs(residuals_hold_BIC/grth_ts_hold))/length(residuals_hold_BIC) 
rmse_hold_BIC; mape_hold_BIC

### Consider the model chosen by auto.arima (model 3b)
f_chosen_auto <- forecast(chosen_auto, h = 6)
point_f_chosen_auto <- f_chosen_auto$mean
point_f_chosen_auto
plot(f_chosen_auto)
residuals_hold_auto <- point_f_chosen_auto - grth_ts_hold
rmse_hold_auto <- sqrt(sum(residuals_hold_auto^2)/length(residuals_hold_auto))
mpae_hold_auto <- sum(abs(residuals_hold_auto/grth_ts_hold))/length(residuals_hold_auto) 
rmse_hold_auto; mpae_hold_auto

# choose the model with the smaller RMSE (two feasible models need to be checked: model 3a-aic & model 3a-bic)
if (rmse_hold_AIC &  rmse_hold_auto > rmse_hold_BIC){
  print('BIC has smaller RMSE')
} else if (rmse_hold_AIC < rmse_hold_BIC & rmse_hold_auto){
  print('AIC has smaller RMSE')
} else if (rmse_hold_auto < rmse_hold_AIC & rmse_hold_BIC){
  print('Auto has smaller RMSE')
} else{
  print('All have same RMSE')
}

#re-estimate the model with lowest RMSE using all the data that you have (1973Q1 to 2020Q2) 
chosen_forecast <- Arima(grth_ts, order = c(5, 0, 5)) 
summary(chosen_forecast)
#forecastfor the true-out-of-sample period. 
f_chosen_final <- forecast(chosen_forecast, h = 6) #2020Q3 - 2021Q4: h = 6 step ahead
f_chosen_final
plot(f_chosen_final)
