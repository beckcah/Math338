library(alr4)
data = oldfaith
plot(Interval ~ Duration, data = data)

library(splines) # need this to run bs function
m1 = lm(Interval ~ bs(Duration, df = 4, degree = 3), data = data)
summary(m1)
x = with(data, seq(min(Duration), max(Duration), length = 100))
lines(x, predict(m1, newdata = data.frame(Duration = x)))

# can't do m2 = lm(Interval ~ bs(Duration, df = 2, degree = 3), data = data)
m2 = lm(Interval ~ bs(Duration, df = 2, degree = 2), data = data)

data = Rateprof
names(data)
data = data[,8:12]
pairs(~., data = data)
z = prcomp(data) # principal component
z # gives regressors (X1 = ENTIRE COLUMN from PC1)
summary(z)
