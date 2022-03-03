library(alr4)
data = fuel2001
names(data)
data = transform(data, Dlic = 1000*Drivers / Pop, Fuel = 1000* FuelC / Pop, Income = Income / 1000, logMiles = log(Miles))
summary(data)
m1 = lm(Fuel~Tax+Dlic+Income+logMiles, data=data)
summary(m1)
plot(Effect("Tax",m1))

data3 = BGSgirls
names(data3)
m3 = lm(BMI18~WT2+WT9+WT18, data=data3)
pairs(~BMI18 + WT2 + WT9 + WT18, data=data3)
summary(m3)
data3 = transform(data3, DT9=WT9-WT2, DT18=WT18-WT9)
names(data3)
m4 = lm(BMI18~WT2+DT9+DT18, data = data3)
summary(m4)
m5 = lm(BMI18~WT2+WT9+WT18+DT9+DT18, data = data3)
summary(m5)


data4 = MinnWater
names(data4)
pairs(~muniUse + muniPrecip+muniPop+year, data=data4)
m1 = lm(muniUse~muniPrecip, data=data4)
m2 = lm(muniUse~muniPrecip+muniPop, data =data4)
m3 = lm(muniUse~muniPrecip+muniPop+year, data=data4)
m4 = lm(muniUse~year, data=data4)
summary(m1)
summary(m2)
summary(m3)
summary(m4)
