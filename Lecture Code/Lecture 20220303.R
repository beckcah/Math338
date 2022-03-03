library(alr4)
data = fuel2001
names(data)
data = transform(data, Dlic = 1000*Drivers / Pop, Fuel = 1000* FuelC / Pop, Income = Income / 1000, logMiles = log(Miles))

m1 = lm(Fuel ~ Tax + Dlic + Income + logMiles, data = data)
m2 = lm(Fuel ~ Tax + Dlic + Income + Miles, data = data)
plot(Effect("logMiles",m1))
plot(Effect("Miles",m2))
pairs(~Fuel+Tax+Dlic+Income+Miles, data=data)
pairs(~Fuel+Tax+Dlic+Income+logMiles, data=data)

data = UN11
m0 = lm(log(fertility)~log(ppgdp)+lifeExpF, data = data)
summary(m0)
100*(exp(-0.028)-1)

100*(exp(log(1.1)*(-.065))-1)
