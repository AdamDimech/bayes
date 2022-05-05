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

#Rcode 3.5
library(rethinking)
dens(samples)

png(filename="plots/SR-3.5-density-estimate.png")
dens(samples)
dev.off()

# Intervals of defined boundaries (Rcode 3.6-3.8)
sum(posterior[p_grid <0.5])
# Output: [1] 0.1718746

sum(samples<0.5)/1e4
# Output: [1] 0.1726

sum(samples > 0.5 & samples < 0.75)/1e4
#Output [1] 0.6054

# Rcode 3.9
quantile(samples, 0.8)
# output:
#      80% 
#0.7597598 

quantile(samples, c(0.1, 0.9))
# Output:
#        10%       90% 
#  0.4504505 0.8158158 

# Grid approximation
p_grid <- seq(from=0, to=1, length.out=1000)
prior <- rep(1, 1000)
likelihood <- dbinom (3, size=3, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample(p_grid, size=1e4, replace=TRUE, prob=posterior)

# Rcode 3.12
PI(samples, prob=0.5)
# Output:
#        25%       75% 
#  0.7087087 0.9299299 

# Rcode 3.13
HPDI(samples, prob=0.5)
#Output:
#     |0.5      0.5| 
#0.8388388 1.0000000 


PI(samples, prob=0.8)
HPDI(samples, prob=0.8)

PI(samples, prob=0.95)
HPDI(samples, prob=0.95)

# Maximum a posteriori Rcode 3.14/15
p_grid[which.max(posterior)]
chainmode(samples, adj=0.01)
#[1] 0.9988292

# Rcode 3.17
sum(posterior*abs(0.5-p_grid))
# [1] 0.3128752

# Rcode 3.18
loss <- sapply(p_grid, function(d) sum (posterior*abs(d-p_grid)))
p_grid[which.min(loss)]
# [1] 0.8408408

#Rcode 3.20
dbinom(0:2, size=2, prob=0.7)

#Rcode 3.21
rbinom(1, size=2, prob=0.7)

#Rcode 3.22
rbinom(10, size=2, prob=0.7)

#Rcode 3.23
dummy_w <- rbinom (1e5, size=2, prob=0.7)
table(dummy_w)/1e5

#Rcode 3.24
dummy_w <- rbinom (1e5, size=9, prob=0.7)
simplehist(dummy_w, xlab="Dummy Water Count")

png(filename="plots/SR-3.24-dummy-water-count.png")
simplehist(dummy_w, xlab="Dummy Water Count")
dev.off()

# Rcode 3.25
w <- rbinom(1e4, size=9, prob=0.6)
simplehist(w)

png(filename="plots/SR-3.25-simulated-predictions.png")
simplehist(w)
dev.off()

