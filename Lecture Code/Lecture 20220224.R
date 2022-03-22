library(alr4)
data = fuel2001
data = transform(data, Dlic = 1000*Drivers / Pop, Fuel = 1000* FuelC / Pop, Income = Income / 1000, logMiles = log(Miles))
m1 = lm(Fuel ~ Tax+Dlic+Income+logMiles, data = data)
summary(m1)

cov(data)
dim(data)
(51-1)*cov(data)
1-deviance(m1)/ 395694