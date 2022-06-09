# Statistical Rethinking: Chapter 4

library(rethinking)

#Rcode 4.1
pos <-replicate(1000, sum(runif(16, -1, 1)))
hist(pos)
plot(density(pos))

png(filename="plots/SR-4.1-histogram-of-positions.png")
hist(pos)
dev.off()

png(filename="plots/SR-4.1-density-positions.png")
plot(density(pos))
dev.off()

#Rcode 4.2
prod(1+runif(12,0,0.1))

#Rcode 4.3
growth <- replicate(1000, prod(1+runif(12,0,0.1)))
dens(growth, norm.comp=TRUE)

png(filename="plots/SR-4.3-normal-by-multiplication.png")
dens(growth, norm.comp=TRUE)
dev.off()

#Rcode 4.4

big <- replicate(10000, prod(1+runif(12,0,0.5)))

png(filename="plots/SR-4.4-normal-by-multiplication-big.png")
dens(big, norm.comp=TRUE)
dev.off()

small <- replicate(10000, prod(1+runif(12,0,0.01)))

png(filename="plots/SR-4.4-normal-by-multiplication-small.png")
dens(small, norm.comp=TRUE)
dev.off()

# Rcode 4.5
log.big <- replicate(10000, log(prod(1+runif(12,0,0.01))))
png(filename="plots/SR-4.4-normal-by-multiplication-log-big.png")
dens(log.big, norm.comp=TRUE)
dev.off()

# Rcode 4.6
w <- 6;
n <- 9;
p_grid <- seq(from=0, to=1, length.out=100)
posterior <- dbinom(w, n, p_grid)*dunif(p_grid, 0, 1)
posterior <- posterior/sum(posterior)
plot(posterior)

# Rcode 4.7-4.11
data(Howell1)
d <- Howell1
str(d)
precis(d, hist=FALSE)
d$height
d2 <- d[d$age >=18, ]

# p.80
png(filename="plots/SR-4.11-density-plot-heights-adults.png")
dens(d2$height)
dev.off()

# Rcode 4.12
png(filename="plots/SR-4.12-mean-prior.png")
curve(dnorm(x, 178, 20), from=100, to=250)
dev.off()

# Rcode 4.13
png(filename="plots/SR-4.13-standard-deviation-prior.png")
curve(dunif(x, 0, 50), from=-10, to=60)
dev.off()

# Rcode 4.14 Sampling from the prior
sample_mu <- rnorm(1e4, 178, 20)
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
png(filename="plots/SR-4.14-sampling-from-the-prior.png")
dens(prior_h)
dev.off()

# Rcode 4.15
sample_mu <- rnorm(1e4, 178, 100)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
png(filename="plots/SR-4.15-sampling-from-the-prior-less-informative.png")
dens(prior_h)
dev.off()

# Rcode 4.16
mu.list <- seq(from=150, to=160, length.out=100)
sigma.list <- seq(from=7, to=9, length.out=100)
post <- expand.grid(mu=mu.list, sigma=sigma.list)
post$LL <- sapply (1:nrow(post), function(i) sum(dnorm(d2$height, post$mu[i], post$sigma[i], log=TRUE)))
post$prod <- post$LL + dnorm(post$mu, 178, 20, TRUE) + dunif(post$sigma, 0, 50, TRUE)
post$prob <- exp(post$prod - max(post$prod))

# Rcode 4.17
png(filename="plots/SR-4.17-contour-plot.png")
contour_xyz (post$mu, post$sigma, post$prob)
dev.off()

# Rcode 4.18
png(filename="plots/SR-4.18-heat-plot.png")
image_xyz (post$mu, post$sigma, post$prob)
dev.off()

# Rcode 4.19

sample.rows <- sample(1:nrow(post), size=1e4, replace=TRUE, prob=post$prob)
sample.mu <- post$mu[sample.rows]
sample.sigma <- post$sigma[sample.rows]

png(filename="plots/SR-4.20-samples-posterior-height-data.png")
plot(sample.mu, sample.sigma, cex=1, pch=16, col=col.alpha(rangi2, 0.1))
dev.off()

