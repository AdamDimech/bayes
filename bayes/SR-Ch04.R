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

# Rcode 4.20

png(filename="plots/SR-4.20-samples-posterior-height-data.png")
plot(sample.mu, sample.sigma, cex=0.6, pch=16, col=col.alpha(rangi2, 0.1))
dev.off()

# Rcode 4.21

png(filename="plots/SR-4.21-density_of_mu.png")
dens(sample.mu)
dev.off()

png(filename="plots/SR-4.21-density_of_sigma.png")
dens(sample.sigma)
dev.off()

# Rcode 4.22

PI(sample.mu)
PI(sample.sigma)

# Rcode 4.23
d3 <- sample(d2$height, size=20)

# Rcode 4.24
mu.list<-seq(from=150,to=170,length.out=200)
sigma.list<-seq(from=4,to=20,length.out=200)
post2<-expand.grid(mu=mu.list,sigma=sigma.list)
post2$LL<-sapply(1:nrow(post2),function(i)
sum(dnorm(d3,mean=post2$mu[i],sd=post2$sigma[i],log=TRUE)))
post2$prod<-post2$LL+dnorm(post2$mu,178,20,TRUE)+dunif(post2$sigma,0,50,TRUE)
post2$prob<-exp(post2$prod-max(post2$prod))
sample2.rows<-sample(1:nrow(post2),size=1e4,replace=TRUE,prob=post2$prob)
sample2.mu<-post2$mu[sample2.rows]
sample2.sigma<-post2$sigma[sample2.rows]

png(filename="plots/SR-4.24-posterior-density-mu.png")
plot(sample2.mu,sample2.sigma,cex=0.5, col=col.alpha(rangi2,0.1), xlab="mu",ylab="sigma",pch=16)
dev.off()

# Rcode 4.25
png(filename="plots/SR-4.25-marginal-posterior-density.png")
dens(sample2.sigma, norm.comp=TRUE)
dev.off()

# Rcode 4.26
d <- Howell1
d2 <- d[d$age>=18,]

# Rcode 4.27
flist <- alist(height ~ dnorm(mu, sigma), mu ~ dnorm(178, 20), sigma ~ dunif(0, 50))

# Rcode 4.28
m4.1 <- quap(flist, data=d2)

# Rcode 4.29
precis(m4.1)

# Rcode 4.31
m4.2 <- quap(
  alist(
    height ~ dnorm (mu, sigma),
    mu ~ dnorm (178, 0.1),
    sigma ~ dunif (0,50)
  ), data=d2 )
precis(m4.2)
