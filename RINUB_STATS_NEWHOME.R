rm(list=ls()) #remove all the variables previously stored
library(fpp2)
library(ggplot2)
library(tseries)
new_home=read.csv("C:\\Users\\Asus TUF\\OneDrive\\Documents\\statistics\\NewHouseRegistrations_Ireland.csv",header = TRUE)
new_home

home<- ts(new_home$NewHouseRegistrations,start = c(1978), end = c(2019) ,frequency = 1)
home

plot(home)
class(home)
abline(reg=lm(home~time(home)))
autoplot(home)

start(home)
end(home)
frequency(home)
cycle(home)

#SEASONAL PLOT
ggseasonplot(home,year.labels = TRUE,year.labels.left = TRUE) + ylab("registration")+ ggtitle("The New home registration")

#smoothing time series
plot(home,main="RAW TIME SERIES")
plot(ma(home,2))
plot(ma(home,4))
autoplot(home)+autolayer(ma(home,2))+autolayer(ma(home,4))

seasonplot(home)


#NAIVE MODEL
fcast.naive<-naive(home,h=3)
summary(fcast.naive)
plot(fcast.naive)

#SEASONAL NAIVE MODEL 
fcast.seasonalnaive<-snaive(home,h=3)
summary(fcast.seasonalnaive)
plot(fcast.seasonalnaive)


#Assess stationarity of the differenced series
adf.test(home)


#checking the order of differencing required
ndiffs(home)
#plot the differenced trips time series
dtrips<-diff(home)
plot(home)

#plot the differenced trips time series
dtrips<-diff(home)

## plot the differenced data
plot(dtrips, ylab = expression(paste(home)))

#Assess stationarity of the differenced series
adf.test(dtrips)

#plot the differenced trips time series
dtrip<-diff(dtrips)
#Assess stationarity of the differenced series
adf.test(dtrip)


#plot the differenced trips time series
dtri<-diff(dtrip)
#Assess stationarity of the differenced series
adf.test(dtr)



#ACF/PACF plots. choosing p and q
Acf(home)
Pacf(home)

#FITTING ARIMA MODEL
fit<-arima(home, order=c(0,1,2))
fit

#EVALUATING THE MODEL FIT
qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals,type="Ljung-Box")
checkresiduals(fit)
accuracy(fit)

#Forecating with the Arima model
forecast(fit,3)
plot(forecast(fit,3),xlab="Year",ylab="Annual Flow")

#auto Arima function
plot(home)
fit<-auto.arima(home)
fit
accuracy(fit)
checkresiduals(fit)



