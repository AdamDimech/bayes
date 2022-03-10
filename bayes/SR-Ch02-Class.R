# Code written as part of Statistical Rethinking class on 2022-03-10

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
#w ~ Binomial (N, p)
# These don't sum to 1 because they are likelihoods, not probabilities
likelihood <- dbinom(water, size=N, prob=pGrid)

#Compute product of likelihood and prior
unstd.posterior <- likelihood * prior

#Standardise the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)


#Save plot

png("plots/SR-2.3-posterior.png", width = 900, height = 500)

plot( pGrid, posterior, type="b", xlab="Probability of Water", ylab = "Posterior probability")

dev.off()