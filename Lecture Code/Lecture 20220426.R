library(alr4)
data = UN11
m = lm(lifeExpF ~ fertility + log(ppgdp), data=data)
wt = abs(m$residuals) #Creating weights from residuals.

m2 = lm(lifeExpF ~ fertility + log(ppgdp), data=data, weights=wt) #simply incorporate the weights to the model
summary(m2) #we end up with different values at different confidence levels when incorporating weights into the model
summary(m)
