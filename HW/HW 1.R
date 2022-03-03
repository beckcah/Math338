#HW 1
library(alr4)

#P1 1.4
summary(oldfaith)
plot(oldfaith$Duration,oldfaith$Interval)
ggplot(oldfaith, aes(x=Duration, y=Interval)) + 
  geom_point() +
  labs(title="Old Faithful Geyser", subtitle="Problem 1.2")

#P2 2.2
#2.22
data = UBSprices
data$ricedif = data$rice2009-data$rice2003
maxi = which.max(ricedif)
row.names(data)[maxi]
data$ricedif[maxi]
mini = which.min(ricedif)
row.names(data)[mini]
data$ricedif[mini]


#P3 2.2
summary(Forbes)
bpKelvin = (5/9)*Forbes$bp+255.37
u1 = 1/bpKelvin
data = data.frame(u1,Forbes$pres)
ggplot(data, aes(x=u1, y=Forbes.pres)) +
  geom_point()+
  labs(title="pres vs. u1", subtitle="Problem 2.71")+
  xlab("u1")+ylab("pres")

m1 = lm(pres~u1, data = Forbes)
summary(m1)


m2 = lm(log(pres)~bp, data = Forbes)

data1 = data.frame(bp=Forbes$bp,fit1=m1$fitted.values,fit2=exp(m2$fitted.values))
data2 = pivot_longer(data1, cols=c("fit1","fit2"), names_to = "fit")

ggplot(data1, aes(x=fit1, y=fit2))+
  geom_point()+
  labs(title="log vs u1", subtitle = "Problem 2.73", x="u1 Fit", y="log Fit")
plot(m1$fitted.values, exp(m2$fitted.values))
ggplot(data2, aes(x=bp, y=value, color=fit))+
  geom_point()+
  labs(title="Comparison of Fits", subtitle = "Problem 2.73", x="bp", y="pres")+
  scale_color_hue(name="Legend", labels=c("u1 Fit","log Fit"))

plot(residuals(m1))
plot(residuals(m2))

bpKelvin2 = (5/9)*Hooker$bp+255.37
u2 = 1/bpKelvin2
m3 = lm(pres~u2, data = Hooker)
data3 = data.frame(u2,Hooker$pres)
ggplot(data3, aes(x=u2, y=Hooker.pres)) +
  geom_point()+
  labs(title="pres vs. u2", subtitle="Problem 2.7.4")+
  xlab("u1")+ylab("pres")
abline(m3)
summary(m3)

#P5 2.13
summary(Heights)
m1 = lm(dheight~mheight, data =Heights)
summary(m1)
CI = c(0.54175-qt(.995,1373)*0.02596,0.54175+qt(.995,1373)*0.02596)

#2.13.3
Sxx = sum((Heights$mheight-mean(Heights$mheight))^2)
sepred = 2.266*(1+1/(1375)+(64-mean(Heights$mheight))^2/Sxx)^(1/2)
PI = c(29.91744+0.54175*64-qt(.995,1373)*sepred,29.91744+0.54175*64+qt(.995,1373)*sepred)


#P6 

m6 = lm(Interval~Duration, data = oldfaith)
summary(m6)
Sxx = sum((oldfaith$Duration-mean(oldfaith$Duration))^2)
sefit = 6.004*(1/270+(250-mean(oldfaith$Duration))^2/Sxx)^(1/2)
CI = c(33.987808+0.176863*250-sefit*(qf(1-.05,2,268))^(1/2),33.987808+0.176863*250+sefit*(qf(1-.05,2,268))^(1/2))

sepred = 6.004*(1+1/(270)+(250-mean(oldfaith$Duration))^2/Sxx)^(1/2)
quant = 33.987808+0.176863*250+qt(1-.1,268)*sepred
