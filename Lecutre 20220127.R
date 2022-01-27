library(alr4)

# Ex 1
plot(mheight~dheight, data = Heights)
m1 = lm(mheight~dheight, data = Heights)
abline(m1)
m1

# Ex 2
plot(bp~pres, data = Forbes)
plot(bp~log(pres), data = Forbes)

# Ex 3
plot(Late~Early, data = ftcollinssnow)
