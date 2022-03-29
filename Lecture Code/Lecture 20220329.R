library(alr4)
data = Highway
data$sigs1 = with(data, (sigs * len + 1)/len) 
names(data)

f = ~ log(len) + shld + log(adt) + log(trks) + lane + slim + lwid + itg + log(sigs1) + acpt + htype

m0 = lm(log(rate)~1, data)
m.forward = step(m0, scope = f, direction = "forward")


predictions = predict(model, data = test)
