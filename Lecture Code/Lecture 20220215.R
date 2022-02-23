library(alr4)

# Plotting Residuals
m1 = lm(lpres~bp, data = Forbes)
plot(predict(m1),residuals(m1))
abline(h=0, lty=2)

#Multiple Linear Regression
data = UN11
names(data)
m1 = lm(lifeExpF~log(ppgdp), data = data)
summary(m1)
m0 = lm(lifeExpF~fertility, data = data)
summary(m0)
m2 = lm(log(ppgdp)~fertility, data = data)
summary(m2)
m3 = lm(fertility~log(ppgdp), data = data)
summary(m3)
m = lm(residuals(m1)~residuals(m3), data = data)
summary(m)
