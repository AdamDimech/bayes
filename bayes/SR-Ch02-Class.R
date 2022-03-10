# Clean up
rm(list=ls())

# Data
ourmeasures <- c("L", "W", "W", "W", "W", "L", "W", "L", "W")

ourMeasuresF <- factor(ourmeasures)

str(ourMeasuresF)

water <- sum(as.numeric(ourMeasuresF)-1)
N <- length(ourMeasuresF)
water
N

Land = N-water

# Grid approximation
G <- 10
pGrid <- seq( from=0, to=1, length.out=G)

#Define prior
#posterior = likelihood * prior / scaling_factor

# Define the prior
prior <- rep(1, G)

#Compute likelihood
likelihood <- dbinom(6, size=water+Land, prob=pGrid)

#Compute product of likelihood and prior
unstd.posterior <- likelihood * prior

#Standardise the posterior, so it sums to 1

posterior <- unstd.posterior / sum(unstd.posterior)

plot( pGrid, posterior, type="b", xlab="Probability of Water", ylab = "Posterior probability")
#mtext( "10 points")