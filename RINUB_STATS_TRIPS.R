rm(list=ls()) #remove all the variables previously stored

library(fpp2)
library(ggplot2)
library(tseries)
overseas_trip=read.csv("C:\\Users\\Asus TUF\\OneDrive\\Documents\\statistics\\OverseasTrips.csv",header = TRUE)
overseas_trip

trips <- ts(overseas_trip$Trips.Thousands.,start = c(2012,1), end = c(2019,4) ,frequency = 4)
trips

class(trips)

plot(trips)
abline(reg=lm(trips~time(trips)))


autoplot(trips)

start(trips)
end(trips)
frequency(trips)

#SEASONAL PLOT
ggseasonplot(trips,year.labels = TRUE,year.labels.left = TRUE) + ylab("Trips")+ ggtitle("The overseas Trips")

#smoothing time series
plot(trips,main="RAW TIME SERIES")
plot(ma(trips,2))
plot(ma(trips,4))
autoplot(trips)+autolayer(ma(trips,2))+autolayer(ma(trips,4))

seasonplot(trips)

#SEASONAL DECOMPOSITION ADDITIVE
fit.decadd<-decompose(trips,type="additive")
fit.decadd
plot(fit.decadd)

#SEASONAL DECOMPOSITION MULTIPLICATIVE
fit.decmult<-decompose(trips,type="multiplicative")
fit.decmult
plot(fit.decmult)

#SEASONAL NAIVE MODEL 
fcast.seasonalnaive<-snaive(trips,h=3)
summary(fcast.seasonalnaive)
plot(fcast.seasonalnaive)

#ETS(SOFTWARE SELECTS THE MODEL)
fitJJ<-ets(trips, model="ZZZ")
fitJJ
forecast(fitJJ,3)
round(accuracy(fitJJ),3)

#ARIMA MODEL

#Assess stationarity of the differenced series
adf.test(trips)
#checking the order of differencing required
ndiffs(trips)
#plot the differenced trips time series
dtrips<-diff(trips)
plot(trips)

#plot the differenced trips time series
dtrips<-diff(dtrips)

#Assess stationarity of the differenced series
adf.test(dtrips)



#ACF/PACF plots. choosing p and q
Acf(dtrips)
Pacf(dtrips)

#FITTING ARIMA MODEL
fit<-arima(trips, order=c(0,1,1))

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
plot(trips)
fit<-auto.arima(trips)
fit
accuracy(fit)
checkresiduals(fit)

