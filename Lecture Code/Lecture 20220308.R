library(alr4)
data = UN11
n = dim(UN11)[1]
levs = pf(99*c(0.01,0.03,0.05,0.07),2,n)-1/n
with(UN11, dataEllipse(pctUrban, log(ppgdp), levels=levs-1/n, xlim=c(-20, 130), ylim = c(3,14)))

data =caution
names(data)
m = lm(y~x1+x2, data=data)
residualPlot(m)
