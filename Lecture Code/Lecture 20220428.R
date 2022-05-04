library(alr4)
#Examining relationships of variance stablizing transformaations
x = c(100:1000)
y = rpois(901, lambda = 0.1*x) #Make a linear-ish relationship
plot(y~x)

m1 = lm(y~x)
residualPlots(m1) #Variance get's larger
ncvTest(m1) #Significant that there is nonconstant variance

#Try to transform the data with a squareroot
m2 = lm(sqrt(y)~x)
plot(sqrt(y)~x) #Not that linear
residualPlots(m2) #Variance looks more evenly distributed even though the fit is bad
ncvTest(m2) #pvalue is much higher
