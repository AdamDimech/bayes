# Define Grid
p_grid <- seq( from=0, to=1, length.out=20 )

#Define Prior
prior <- rep(1, 20)

#Compute likelihood
likelihood <- dbinom(6, size=9, prob=p_grid)

#Compute product of likelihood and prior
unstd.posterior <- likelihood * prior

#Standardise the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot( p_grid, posterior, type="b", xlab="Probability of Water", ylab = "Posterior probability")
mtext( "20 points")

# Save plot as PNG

png(filename="plots/SR-2.3-posterior.png")
plot( p_grid, posterior, type="b", xlab="Probability of Water", ylab = "Posterior probability")
mtext( "20 points")
dev.off()

# Compute the quadratic approximation to the globe tossing data (R code 2.6)

library(rethiking)

globe.qa <- quap(
  alist(
    W ~ dbinom(W+L, p), #binomial likelihood
    p ~ dunif(0,1) #uniform prior
  ),
  data=list(W=6, L=3) )

# Display summary of quadratic approximation
precis( globe.qa)

# Output:
#   mean   sd 5.5% 94.5%
# p 0.67 0.16 0.42  0.92
# This means that the posterior has a mean value of 0.67 and the curvature (sd) is the
# standard deviation of the posterior distribution.

# Analytical calculation (Rcode 2.7)
W <- 6
L <- 3
curve ( dbeta (x, W+1, L+1), from=0, to=1 )

# Quadratic approximation
curve ( dnorm( x, 0.67, 0.16), lty=2, add=TRUE)


# Save the plot as PNG
png(filename="plots/SR-2.7-quadratic-approximation.png")
curve ( dbeta (x, W+1, L+1), from=0, to=1 )
curve ( dnorm( x, 0.67, 0.16), lty=2, add=TRUE)
dev.off()



