# Statistical Rethinking: Chapter 4

library(rethinking)

# Rcode 5.1

data(WaffleDivorce)
d <- WaffleDivorce

d$D <- standardize(d$Divorce)
d$M <- standardize(d$Marriage)
d$A <- standardize(d$MedianAgeMarriage)

# Rcode 5.2
sd(d$MedianAgeMarriage)

# Compute the approximate prior (Rcode 5.3)
m5.1 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA * A,
    a ~ dnorm(0,0.2),
    bA ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data=d
)

