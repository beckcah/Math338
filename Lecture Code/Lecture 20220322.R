library(alr4)
data = Forbes
m = lm(lpres ~ bp, data = data)
pairs(~lpres+bp, data=data)
summary(m)
residuals(m)
hatvalues(m)
#p-value
2*(1-pt(12.41,df=14))

#In one line
outlierTest(m)
dim(data)


data = UN11
names(data)
m = lm(fertility ~ log(ppgdp)+lifeExpF, data = data)
betanoi = influence(m)$coefficients
betanoi
pairs(betanoi)


data = rat
names(data)
m = lm(y~BodyWt+LiverWt+Dose, data =data)
pairs(~y+BodyWt+LiverWt+Dose, data = data)
summary(m)
cooks.distance(m)
influenceIndexPlot(m)
data = data[-3,]
m = lm(y~BodyWt+LiverWt+Dose, data =data)
summary(m)
