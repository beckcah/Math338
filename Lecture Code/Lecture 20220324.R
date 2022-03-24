library(alr4)
data = rat
names(data)
m = lm(y~BodyWt+LiverWt+Dose, data=data)
qqPlot(m)

ks.test(residuals(m),rnorm(dim(data)))

ks.test(residuals(m),"pnorm")

data = MinnWater
names(data)
m0 = lm(log(muniUse)~year, data=data)
summary(m0)
pairs(~log(muniUse)+year+log(muniPop), data=data)
m1 = lm(log(muniUse)~year+log(muniPop),data=data)
summary(m1)
