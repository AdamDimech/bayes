# Statistical Rethinking: Chapter 3

# Rcode 3.1
Pr_Positive_Vampire <- 0.95
Pr_Positive_Mortal <- 0.01
Pr_Vampire <- 0.001
Pr_Positive <- Pr_Positive_Vampire * Pr_Vampire + Pr_Positive_Mortal * (1-Pr_Vampire)
(Pr_Vampire_Positive <- Pr_Positive_Vampire*Pr_Vampire / Pr_Positive)

# Output: [1] 0.08683729
# = 87% chance

#Rcode 3.2

p_grid <- seq( from=0, to=1, length.out=1000 )
prob_p <- rep(1, 1000)
prob_data <- dbinom(6, size=9, prob=p_grid)
posterior <- prob_data*prob_p
posterior <- posterior/sum(posterior)

# Rcode 3.3/3.4
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)
plot(samples)

# Save plot
png(filename="plots/SR-3.4-sampling-parameter-values.png")
plot(samples)
dev.off()
