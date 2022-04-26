library(alr4)

#9.3
data = pipeline
names(pipeline)
plot(data$Lab,data$Field)

m1 = lm(Lab~Field, data=data)
summary(m1)
residualPlot(m1)
ncvTest(m1)


#9.8
data = water
names(water)
m2 = lm(log(BSAAM)~log(APMAM)+log(APSAB)+log(APSLAKE)+log(OPBPC)+log(OPRC)+log(OPSLAKE), data=data)

residualPlots(m2, ~.,tests=TRUE)

residualPlots(m2, ~1,tests=TRUE,plot=FALSE)

#9.11
#First we will intitialize our dataframe and then fill it iwh thte given data from the regression
data = data.frame(matrix(ncol=4,nrow=5))
colnames(data) = c("State","Fuel","residual","hat")
data$State <- c("Alaska","New York","Hawaii","Wyoming","District of Columbia")
data$Fuel <- c(514.279,374.164,426.349,842.792,317.492)
data$residual <- c(-163.145,-137.599,-102.409,183.499,-49.452)
data$hat <- c(0.256,0.162,0.206,0.084,0.415)
#These values come from the regression analysis. Pprime is the number of regressors
#plus 1 for the itnercept, which we see is 5
sigmahat = 64.891
dof = 46
pprime = 5
#First we can calculate the values of r for each data point
data = transform(data, r = residual/(sigmahat * sqrt(1-hat)))
#then with this value we can get the t statistic for each data point
data = transform(data, t = r*sqrt((46-1)/(46-r^2)))
#Also we the value of r and pprime we can calculate the Cook's Distance for each point
data = transform(data, D = 1/pprime*r^2*hat/(1-hat))
#Finally we calculate p values based on the size of data for each data point.
#This is effectively doing an outlier test
data = transform(data, p = pmin(2*(dof+pprime)*pt(-abs(t),dof),1))
print(data)
#looking at our data analysis we see that all of the p values and t statistics are relatively large,
#so there is not much significance that our data points given are outliers. Also, the Cook's distance
#for each data point is less than 1, another metric we can use to determine outliers.
#Even with this fact we see that Alaska is the most influential with the Cook's Distance of 0.584

#10.3
data = mantel
names(data)

m0 = lm(Y~1, data=data)
m.forward = step(m0, scope = ~X1+X2+X3, direction = "forward")

m1 = lm(Y~X1+X2+X3, data=data)
m.backward = step(m1, scope ~1, direction = "backward")

summary(m.forward)
summary(m.backward)

AIC(m.forward)
AIC(m.backward)

BIC(m.forward)
BIC(m.backward)


