library("x12")
library(fpp)
library("TTR")
library("tsoutliers")
rm(list=ls())
setwd("C:/Users/amustaki/Documents/mspa")


sold_data<-read.csv("SFH_Sold.csv")
starts_data<-read.csv("SFH_Starts.csv")

sold <- ts(sold_data, frequency=12, start=c(1976,1))
starts <- ts(starts_data, frequency=12, start=c(1976,1))
str(sold)
plot(sold)
sold_R1<-window(sold,start=1976,end=1982) #regime1
sold_R2<-window(sold,start=1982,end=1986)#regime2
plot(sold_R1)
plot(sold_R2)

plot(log(sold_R2))
plot(log(sold_R1))
#not much difference is log transformed #not sure


tso(sold_R1)
# Series:  
#   ARIMA(1,0,0)(1,0,0)[12] with non-zero mean 
# 
# Coefficients:
#   ar1    sar1     mean
# 0.8274  0.5330  42.3138
# s.e.  0.0622  0.1067   4.5914
# 
# sigma^2 estimated as 15.57:  log likelihood=-204.9
# AIC=417.79   AICc=418.38   BIC=426.95
# 
# No outliers were detected.

tso(sold_R2)
# ARIMA(0,1,0)(1,0,0)[12]                    
# 
# Coefficients:
#   sar1
# 0.6381
# s.e.  0.1090
# 
# sigma^2 estimated as 22.03:  log likelihood=-144.95
# AIC=293.91   AICc=294.17   BIC=297.65
# 
# No outliers were detected.

#Diff?
adf.test(sold_R1) #p<0.05 means stationary which menas differencing NOT needed
# Augmented Dickey-Fuller Test
# 
# data:  sold_R1
# Dickey-Fuller = -2.9122, Lag order = 4, p-value = 0.2037
# alternative hypothesis: stationary
# p > 0.05 means non-stationary, means diff needed

adf.test(sold_R2) #p<0.05 means stationary which menas differencing NOT needed
# Augmented Dickey-Fuller Test
# 
# data:  sold_R2
# Dickey-Fuller = -2.9614, Lag order = 3, p-value = 0.1894
# alternative hypothesis: stationary
# p>0.05 means non-stationary which means differencing  needed

kpss.test(sold_R1) # p<0.05 differencing needed
# KPSS Test for Level Stationarity
# 
# data:  sold_R1
# KPSS Level = 0.45457, Truncation lag parameter = 1, p-value = 0.05363
# diff  needed (Agrees with adf.test)

kpss.test(sold_R2)
# KPSS Test for Level Stationarity
# 
# data:  sold_R2
# KPSS Level = 1.1853, Truncation lag parameter = 1, p-value = 0.01
# 
# Warning message:
#   In kpss.test(sold_R2) : p-value smaller than printed p-value
# no diff needed - does not agrees with adf.test

ndiffs(sold_R1)
# [1] 0
# does not agree with adf or kpss.test

ndiffs(sold_R2)
# [1] 1

tsdisplay(sold_R1)
tsdisplay(diff(sold_R1))

fit_R1<-auto.arima(diff(sold_R1))
summary(fit_R1)
# 
# ARIMA(0,0,0)(2,0,0)[12] with zero mean     
# 
# Coefficients:
#   sar1   sar2
# 0.4123  0.245
# s.e.  0.1308  0.148
# 
# sigma^2 estimated as 15.82:  log likelihood=-203.43
# AIC=412.87   AICc=413.22   BIC=419.7
# 
# Training set error measures:
#   ME     RMSE      MAE MPE MAPE      MASE       ACF1
# Training set 0.1222986 3.922391 3.169999 NaN  Inf 0.9100477 -0.1507124


fit_R2<-auto.arima(diff(sold_R2))
summary(fit_R2)
# ARIMA(0,0,0)(1,0,0)[12] with zero mean     
# 
# Coefficients:
#   sar1
# 0.6381
# s.e.  0.1090
# 
# sigma^2 estimated as 22.03:  log likelihood=-144.95
# AIC=293.91   AICc=294.17   BIC=297.65
# 
# Training set error measures:
#   ME     RMSE      MAE MPE MAPE      MASE       ACF1
# Training set -0.2424486 4.643938 3.726651 NaN  Inf 0.9252375 -0.1425302



Acf(residuals(fit_R1)) #no more spikes - stationary
Box.test(residuals(fit_R1),fitdf=2,lag=2,type="Ljung") #p-value < 0.05 --> non-stationary
# Box-Ljung test
# 
# data:  residuals(fit_R1)
# X-squared = 2.5654, df = 0, p-value < 2.2e-16

tsdiag(fit_R1) #residuals sinosudal, ACF spike at 0, p-values above 0.2 for all lags
checkresiduals(fit_R1)
plot(forecast(fit_R1))


Acf(residuals(fit_R2)) #no spikes
Box.test(residuals(fit_R2),type="Ljung") #p-value > 0.05 --> stationary
# Box-Ljung test
# 
# data:  residuals(fit_R2)
# X-squared = 1.0374, df = 1, p-value = 0.3084 #so stationary

tsdiag(fit_R2) #residuals sinosudal, ACF spike at 0, p-values above 0.05 for all lags
checkresiduals(fit_R2)
plot(forecast(fit_R2))

###############################
#This might work better with Holt-Winter

fit_HR2 <- ets(sold_R2, damped=FALSE)
summary(fit_HR2)
# ETS(M,N,A) 
# 
# Call:
#   ets(y = sold_R2, damped = FALSE) 
# 
# Smoothing parameters:
#   alpha = 0.7057 
# gamma = 1e-04 
# 
# Initial states:
#   l = 53.9934 
# s=-15.273 -8.4536 -3.8309 -1.8982 5.4843 5.8374
# 6.1533 9.4671 7.6836 6.9873 -3.5883 -8.569
# 
# sigma:  0.0578
# 
# AIC     AICc      BIC 
# 325.7000 340.2455 354.0773 
# 
# Training set error measures:
#   ME     RMSE      MAE       MPE    MAPE     MASE        ACF1
# Training set -0.4690035 3.271277 2.383967 -1.154552 4.51113 0.282714 -0.04610294
plot(forecast(fit_HR2))


tsdiag(fit_HR2) #residuals sinosudal, ACF no spikes, p-values above 0.05 for all lags
checkresiduals(fit_HR2) #residuals more normally distributed