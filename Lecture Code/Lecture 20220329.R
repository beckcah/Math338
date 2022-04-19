library(alr4)
data = Highway
data$sigs1 = with(data, (sigs * len + 1)/len) 
names(data)

f = ~ log(len) + shld + log(adt) + log(trks) + lane + slim + lwid + itg + log(sigs1) + acpt + htype

m0 = lm(log(rate)~1, data)
m.forward = step(m0, scope = f, direction = "forward")

m1 = update(m0,f)
m.backward = step(m1, scope = ~1, direction = "backward")

m.stepup = step(m0, scope = f, direction = "both")
m.stepdown = step(m1, scope = ~1, direction = "both")

m2 = update(m0, ~shld+itg+lane+log(trks))

m.stepwise = step(m2, scope = ~1, direction = "both")

m.stepwise = step(m2, scope = f, direction = "both")