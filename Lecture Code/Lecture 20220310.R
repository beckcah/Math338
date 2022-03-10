library(alr4)
data = fuel2001
names(data)
data = transform(data, Dlic= 1000*Drivers/Pop, Fuel = 1000*FuelC/Pop, Income=Income/1000, logMiles = log(Miles))
m = lm(Fuel ~ Tax + Dlic+Income+logMiles, data = data)
residualPlots(m)

m2 = lm(fertility ~ log(ppgdp)+pctUrban, data = UN11)
residualPlots(m2)
m3 = lm(fertility ~ log(ppgdp) + pctUrban + log(ppgdp) * pctUrban, data = UN11)
residualPlots(m3)
m4 = lm(fertility ~ log(ppgdp) + pctUrban + ppgdp + log(ppgdp) * pctUrban, data = UN11)
residualPlots(m4)

data = sniffer
names(data)
m = lm(Y~., data = data)
residualPlots(m)
ncvTest(m)
ncvTest(m, ~TankTemp)
ncvTest(m, ~TankTemp+GasTemp)

ncvTest(m2)
residualPlots(m2)

data = Forbes
m = lm(lpres ~bp, data = data)
names(data)
data = Forbes
outlierTest(m)
pairs(~lpres + bp, data=data)
summary(m)
residuals(m)
hatvalues(m)

2*(1-pt(12.41,14))
