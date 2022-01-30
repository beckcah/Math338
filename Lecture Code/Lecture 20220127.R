library(alr4)
# 1. Introduction

# Ex 1
plot(mheight~dheight, data = Heights)
m1 = lm(mheight~dheight, data = Heights)
abline(m1)
m1

# Ex 2
plot(bp~pres, data = Forbes)
plot(bp~log(pres), data = Forbes)

# Ex 3
plot(Late~Early, data = ftcollinssnow)

# Ex 4
plot(y1~x1, data=anscombe)
plot(y2~x2, data=anscombe)
plot(y3~x3, data=anscombe)
plot(y4~x4, data=anscombe)

# Ex 5
data = transform(fuel2001, Dlic = 1000 * Drivers/Pop, Fuel = 1000 * FuelC/Pop, Income = Income/1000)
pairs(~Fuel + Tax + Dlic + Income + log(Miles), data = data)
