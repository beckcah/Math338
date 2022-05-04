#My last ever R notebook? At least for lecture I suppose...

library(alr4)
data = Transact
dim(data)
names(data)
pairs(~., data=data) #Scatter plot of the variables. Colinearity of t1 and t2. t1 has many values near 0/outliers

m = lm(time ~t1+t2, data=data) #Try a linear regression
summary(m)

attach(data) #need to do this in order to get it to work for some reason
b = Boot(m, R = 1000) #bootstrap this with 1000 points
confint(b) #Can get confidence intervals

#Define a new function to do things
ratio = function(m){
  r = coef(m)[2]/coef(m)[3];
  return(r);
}
b2 = Boot(m, f = ratio, R = 1000)
confint(b2) #This is b1hat/b2hat

b3 = Boot(m, f= ratio, R = 1000, method = "residual")
confint(b3)
