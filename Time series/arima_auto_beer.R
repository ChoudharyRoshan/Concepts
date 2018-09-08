#ts
#diff(act data, differences= //)
#acf(diff data, log.max = //)
#pacf(diff data, log.max = //)
#arima(act data, order=c(//,//,//))
#forecast(aimamod, h=//)

###
library(fpp)
data <- ausbeer
View(data)
class(ausbeer)
plot(data)

#to get diff
data.diff <- diff(data, differences = 3)
plot(data.diff)
mean(data.diff)

#step 2
acf(data.diff, log.max =100)

#step 3
pacf(data.diff, log.max =100)

#step 4
model <- arima(data, order=c(4,3,4),
               seasonal = list(order = c(1L, 3L, 2L), period = 12))
plot(model)

#step5
p1 <- forecast(model, h=10)
plot(p1)


?arima
#Try shortcut by auto.arima
auto_arema <- auto.arima(data)
names(auto_arema)

p2 <- forecast(auto_arema, h=10)
plot(p2)

#Arrange
gridExtra::grid.arrange(p1,p2,ncol=2)
library(grid)
library(gridExtra)
p3 <- grid.arrange(p1,p2,ncol=2)
p3
