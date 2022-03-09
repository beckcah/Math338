library(alr4)
#Problem 3.3
# 3.3.1
data = BGSgirls
data1 = subset(data, select=c("HT2","HT9","WT2","WT9","ST9","BMI18"))
pairs(~HT2+HT9+WT2+WT9+ST9+BMI18, data = data)
cor(data1)
cov(data1)

#3.3.3
m1 = lm(BMI18~HT2+WT2+HT9+WT9+ST9, data=data)
summary(m1)

#4.1
data = transform(data, ave = (WT2+WT9+WT18)/3, lin = WT18-WT2, quad = WT2-2*WT9+WT18)
m2 = lm(BMI18~ave+lin+quad, data=data)
summary(m2)
m3 = lm(BMI18~WT2+WT9+WT18, data=data)
summary(m3)

#4.7
data1 = UN11
m4 = lm(log(fertility)~log(ppgdp)+lifeExpF, data=data1)
summary(m4)
log(.25)
