library(alr4)
data = Rateprof
names(data)
dim(data)
sample = c(sample(1:366,200))
sample
train = data[sample,]
test = data[-sample,]
m0 = lm(quality ~1, data = data)
f = ~ raterInterest + easiness + numYears
step(m0, scope = f)
m = lm(quality ~ easiness+raterInterest, data = train)
prediction = predict(m, newdata = test)
sqrt(mean(prediction - test$quality)^2)

#install.packages("caret")
library(caret)
model = train(quality ~ easiness + numYears, data = data, method = "lm", trControl = trainControl(method = "cv", number = 10, savePredictions = TRUE))
model$results
model$pred

data = UN11
boxplot(lifeExpF ~ group, data = data)
Boxplot(lifeExpF ~ group, data = data)
m = lm(lifeExpF ~ group, data = data)
summary(m)

        