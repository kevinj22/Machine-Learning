library(quantmod)
library(ggplot2)
library(ggfortify)
library(forecast)

# Get Stock Closing Prices

getSymbols('BNS')

# Format as univarite TS
BNS <-BNS$BNS.Close

# Make an ARIMA model on Data

d.arima<- auto.arima(BNS)
d.forecast<-forecast(d.arima, level=c(80), h=50)
autoplot(d.forecast)

