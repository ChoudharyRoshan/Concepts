#ts
#diff(act data, differences= //)
#acf(diff data, log.max = //)
#pacf(diff data, log.max = //)
#arima(act data, order=c(//,//,//))
#forecast(aimamod, h=//)

###
library(fpp)
data <- AirPassengers
View(data)
class(AirPassengers)
plot(data)


data.diff <- diff(data, differences = 2)
plot(data.diff)
mean(data)

#step 2
acf(data.diff, log.max =400)

#step 3
pacf(data.diff, log.max =400)

#step 4
model <- arima(data, order=c(1,2,1))
plot(model)

#step5
p <- forecast(model, h=10)
plot(p)


#Try shortcut by auto.arima

