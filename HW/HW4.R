library(alr4)


#5.4.1
data = MinnLand
boxplot(log(acrePrice)~year, data=data)
#The method matches vaugely the expected nature of the increase from 2002-2006 but does not 
#show the decrease as much after 2007. This decrease begins to show a bit later in the bottom
#of the box portion of the box plot and general outliers, but it appears that Minnesota farm 
#sales did not show a fall in property value.

#5.4.2
m = lm(log(acrePrice)~as.factor(year), data=data)
summary(m)
#Examining the summary we see significant p-values for all years except for 2003. 
#As 2002 would be the intercept we would intepret and say that there was no significant 
#change between the prices in 2002 and the prices in 2003. 

#Interpreting the intercept, we would say in 2002 the estimated log(acrePrice) was 7.27175. 
#For all years other than 2002 and 2003, we have siginicant evidence to say there is an increase 
#by the amount listed under estimate for each over the intercept estimate

#5.8.1
data = cakes
summary(data)
m1 = lm(Y~ X1+X2+I(X1^2)+I(X2^2)+X1*X2, data=data) # Raw
summary(m1)
m2 = lm(Y ~ poly(X1,X2,degree = 2), data=data) #Orthogonal
summary(m2)

#Both summaries give results with parameters for p<0.05 but give significantly different parameters.
#As we expect due to the styles of computation, the models are significant for both styles of fitting,
#but with different parameters.

#5.9.1
library(splines)
data = salarygov
summary(data)
plot(data$Score,data$MaxSalary)
#With this plot we see that there is variation and interesting curvature and has a horn-shape. 
#Additionally there seems to be more data points for lower values of score vs higher values of score

m1 = lm(MaxSalary ~ bs(Score,df=4, d = 3), data=data)
summary(m1)
m2 = lm(MaxSalary ~ bs(Score,df=5, d = 3), data=data)
summary(m2)
m3 = lm(MaxSalary ~ bs(Score,df=10, d = 3), data=data)
summary(m3)

#6.9
data = cakes
m = lm(Y ~ X1+I(X1^2)+X2+I(X2^2)+X1:X2, data = data)
m1 = lm(Y ~ X1+I(X1^2)+X2+I(X2^2), data = data)
anova(m1,m)
#From the anova test we would reject the null hypothesis at the 0.05 level and say beta5 is not 0.
m2 = lm(Y ~ X1+X2+I(X2^2)+X1:X2, data = data)
anova(m2,m)
#From the anova test we reject the null at the 0.05 level and say beta2 is not 0
m3 = lm(Y ~ X2+I(X2^2), data=data)
m4 = lm(Y ~ X1 + X2 + I(X2^2), data=data)
anova(m3,m4)
#From the test of the null and our alternative of adding back in the beta1 term,
#we see at 0.05 significance that beta1 is not 0.
#Thus, we reject the null and conclude at the 0.05 that the alternative hypothesis 
#that not all of the terms are 0, as one term (beta1) is not zero.
