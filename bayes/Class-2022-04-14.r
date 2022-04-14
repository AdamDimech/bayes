# PFG class hosted by Josquin on 2022-04-14

G <- 1000

N = 2000
observations <- rbinom(N, 1, 0.3)
water=as.numeric(sum(observations))
Land=N-water

# Define grid
pGrid <- seq( from=0, to=1, length.out=G)

#prior <- rep(1, G)
prior <- ifelse (pGrid < 0.5, 0.01, 0.09)


# W ~ Binomial (N, p)
likelihood <- dbinom(water, size = water+Land, prob = pGrid)

# Get max value
max(likelihood)

# Get index
which(likelihood==max(likelihood))


# Compute likelihood of and prior
unstdPosterior <- likelihood * prior

# Standardise posterior
posterior <- unstdPosterior / sum(unstdPosterior)


plot( pGrid, posterior, type="b", xlab="Probability of water", ylab="Posterior probability")

plot( pGrid, prior, type="b", xlab="Probability of water", ylab="Posterior probability")

# What is the most likely value of p(water)
which(posterior %in% c(max(posterior)))/G