library(alr4)

names(Forbes)
cov(Forbes)
dim(Forbes)
# To Calculate Sxx, Syy (for instance bp*bp, lpres*lpres, bp*lpres)
(17-1)* cov(Forbes)
# Calculating the sample means.
colMeans(Forbes)
