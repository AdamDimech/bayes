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