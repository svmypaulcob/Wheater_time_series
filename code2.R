data=read.csv('Test.csv')  # read csv data file
View(data) # help to view data into separate tab

is.null(data) # Check if data frame is NULL

class(data$date)
data$date=as.Date(data$date, format =  "%Y-%m-%d" )

df=data[c("date","meantemp")]
class(df)
library(readxl)
library(forecast)
library(tseries)
df1=ts(df$meantemp,start = min(df$date),end = max(df$date))
class(df1)
plot(df1)

acf(df1)
pacf(df1)
adf.test(df1)
# Non-stationary data

dfmodel=auto.arima(df1,ic="aic",trace = TRUE)
# ARIMA(2,1,2) best model
acf(ts(dfmodel$residuals))
pacf(ts(dfmodel$residuals))

dfforecast=forecast(dfmodel,level = c(95),h=10*12)
dfforecast


Box.test(dfforecast$resid, lag=5, type= "Ljung-Box")
Box.test(dfforecast$resid, lag=15, type= "Ljung-Box")
Box.test(dfforecast$resid, lag=25, type= "Ljung-Box")