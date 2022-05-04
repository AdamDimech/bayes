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

# Add more samples (n=36, w = 24)

globe.qa2 <- quap(
  alist(
    W ~ dbinom(W+L, p), #binomial likelihood
    p ~ dunif(0,1) #uniform prior
  ),
  data=list(W=24, L=12) )

precis( globe.qa2)

W <- 24
L <- 12
curve ( dbeta (x, W+1, L+1), from=0, to=1 )
curve ( dnorm( x, 0.67, 0.08), lty=2, add=TRUE)


png(filename="plots/SR-2.7-quadratic-approximation-more-samples.png")
curve ( dbeta (x, W+1, L+1), from=0, to=1 )
curve ( dnorm( x, 0.67, 0.08), lty=2, add=TRUE)
dev.off()

# Markov chain Monte Carlo (Rcode 2.8)

n_samples <- 1000
p <- rep( NA , n_samples )
p[1] <- 0.5
W <- 6
L <- 3
for ( i in 2:n_samples ) {
  p_new <- rnorm( 1 , p[i-1] , 0.1 )
  if ( p_new < 0 ) p_new <- abs( p_new )
  if ( p_new > 1 ) p_new <- 2 - p_new
  q0 <- dbinom( W , W+L , p[i-1] )
  q1 <- dbinom( W , W+L , p_new )
  p[i] <- ifelse( runif(1) < q1/q0 , p_new , p[i-1] )
  }

dens(p, xlim=c(0,1))
curve (dbeta(x, W+1, L+1), lty=2, add=TRUE)

# Save plot

png(filename="plots/SR-2.8-markov-chain-monte-carlo.png")
dens(p, xlim=c(0,1))
curve (dbeta(x, W+1, L+1), lty=2, add=TRUE)
dev.off()
