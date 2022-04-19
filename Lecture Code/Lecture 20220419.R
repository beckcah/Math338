library(alr4)

data = Rateprof
data = data[,8:12]
z = prcomp(data)
z
summary(z)
data

data2 = 100 * data[,1]
data2 = as.matrix(data2, ncol = 5)
z = prcomp(data2)
z
dim(data2)

data = Forbes
names(data)
m = lm(lpres~bp, data=data)
deviance(m) #Finds RSS
dim(data) #Finds n
summary(m)
(17-1)*cov(data) #Helps find SYY

pf(0.95, 1, 15)

data = Wool
dim(data)
m = lm(log(cycles)~ + as.factor(len)+as.factor(amp)+as.factor(load),data=data) #As factor treats each one as a factor as opposed to numerical data
summary(m)
deviance(m)
(27-1)*cov(data) #Need to take log of covariance if we want SYY
data = transform(data, lc=log(cycles))
(27-1)*cov(data) #Transformed data

data = UN11
m1 = lm(lifeExpF ~ group + log(ppgdp), data=data)
m2 = lm(lifeExpF ~ group + log(ppgdp) + group:log(ppgdp), data=data)
dim(data) #Find n
deviance(m1) #Reduced Model
deviance(m2) #Full model
