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

# Simulate from the priors (Rcode 5.4)
set.seed(10)
prior <- extract.prior(m5.1)
mu <- link(m5.1, post=prior, data=list(A=c(-2,2)))
png(filename="plots/SR-5.4-simulate-from-priors.png")
plot(NULL, xlim=c(-2,2), ylim=c(-2,2))
for(i in 1:50) lines(c(-2, 2), mu[i,], col=col.alpha("black", 0.4))
dev.off()

# Posterior predictions (Rcode 5.5)
## compute percentile interval of mean
A_seq <- seq(from=-3 , to=3.2 , length.out=30)
mu <- link(m5.1, data=list(A=A_seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

## plot it all
png(filename="plots/SR-5.5-posterior-predictions.png")
plot(D ~ A, data=d, col=rangi2)
lines(A_seq, mu.mean, lwd=2)
shade(mu.PI, A_seq)
dev.off()


