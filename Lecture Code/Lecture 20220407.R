library(alr4)
m = lm(lifeExpF ~ group, data = UN11)
summary(m)

#Variance covariance matrix is Variance of BetaHat (sigma^2hat*(X^tX)^-1)
vcov(m)
attach(UN11)
pairwise.t.test(lifeExpF, group, p.adj = "bonf",)
a = aov(lifeExpF ~ group)
TukeyHSD(a)

# * in regression combines for all interaction terms
m2 = lm(lifeExpF ~ log(ppgdp) * group, data=UN11)
summary(m2)
#Effect plots examine interaction terms
plot(Effect(c("group", "ppgdp"),m2),multiline = TRUE)

data = transform(UN11, lppgdp = log(ppgdp))
#Lines should be parallel if there is no interaction
m3 = lm(lifeExpF ~ lppgdp*group, data = data)                 
plot(Effect(c("group","lppgdp"),m3),multiline = TRUE)

m4 = lm(lifeExpF ~ log(ppgdp)+group, data = data)
summary(m4)
m4 = lm(lifeExpF ~ lppgdp+group, data = data)
plot(Effect(c("group","lppgdp"),m4), multiline = TRUE)

#inividual choosen interactions
m5 = lm(lifeExpF ~ group + log(ppgdp)+group:log(ppgdp), data = data)
summary(m5)

#Many Terms
data = Wool
names(data)
m6 = lm(log(cycles)~as.factor(len) * as.factor(amp) * as.factor(load), data = data)
summary(m6)
#Too many factors cause overfitting which means we cannot get a regression output




