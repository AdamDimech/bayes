# Statistical Rethinking: Chapter 4

library(rethinking)

# Rcode 5.1

data(WaffleDivorce)
d <- WaffleDivorce

d$D <- standardize(d$Divorce)
d$M <- standardize(d$Marriage)
d$A <- standardize(d$MedianAgeMarriage)