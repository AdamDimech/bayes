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

# Rcode 4.32
vcov(m4.1)

# Rcode 4.33
diag(vcov(m4.1))
cov2cor(vcov(m4.1))

# Rcode 4.34 (Sampling vectors of values from a multi-dimensional Gaussian distribution)
post <- extract.samples(m4.1, n=1e4)

head(post) # View head
write.csv(post, "tables/4.34_post.csv") # Write to CSV

# Rcode 4.35
precis(post)

png(filename="plots/SR-4.35-quap-posterior-post.png")
plot(post)
dev.off()

precis(m4.1)

# Rcode 4.36
library (MASS)
post <- mvrnorm(n=1e4, mu=coef(m4.1), Sigma=vcov(m4.1))
precis(post)

png(filename="plots/SR-4.36-mvrnorm.png")
plot(post)
dev.off()

head(post)
write.csv(post, "tables/4.36_post_mvrnorm.csv")

## 4.4 Linear Prediction ##

# Rcode 4.37

data(Howell1); d <- Howell1; d2 <- d[d$age >= 18, ]

png(filename="plots/SR-4.37-plot-adult-height-weight.png")
plot(d2$height ~ d2$weight)
dev.off()

# Rcode 4.38

set.seed(2971)
N <- 100
a <- rnorm(N, 178, 20)
b <- rnorm(N, 0, 10)

# Rcode 4.39

png(filename="plots/SR-4.39-height-weight-alpha-beta.png")
plot( NULL , xlim=range(d2$weight) , ylim=c(-100,400) ,
      xlab="weight" , ylab="height" )
abline( h=0 , lty=2 )
abline( h=272 , lty=1 , lwd=0.5 )
mtext( "b ~ dnorm(0,10)" )
xbar <- mean(d2$weight)
for ( i in 1:N ) curve( a[i] + b[i]*(x - xbar) ,
                        from=min(d2$weight) , to=max(d2$weight) , add=TRUE ,
                        col=col.alpha("black",0.2) )
dev.off()

# Rcode 4.40

b <- rlnorm(1e4, 0, 1)

png(filename="plots/SR-4.40-log-normal-beta.png")
dens(b, xlim(c(0, 5)), adj=0.1)
dev.off()

# Rcode 4.41

set.seed(2971)
N <- 100
a <- rnorm(N, 178, 20)
b <- rnorm(N, 0, 1)

plot( NULL , xlim=range(d2$weight) , ylim=c(-100,400) ,
      xlab="weight" , ylab="height" )
abline( h=0 , lty=2 )
abline( h=272 , lty=1 , lwd=0.5 )
mtext( "b ~ dnorm(0,10)" )
xbar <- mean(d2$weight)
for ( i in 1:N ) curve( a[i] + b[i]*(x - xbar) ,
                        from=min(d2$weight) , to=max(d2$weight) , add=TRUE ,
                        col=col.alpha("black",0.2) )

# Rcode 4.42

data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

xbar <- mean(d2$weight)

m4.3 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*( weight - xbar ) ,
    a ~ dnorm( 178 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=d2 )

# Rcode 4.44

precis(m4.3)

# Rcode 4.45

round(vcov(m4.3), 3)

# Rcode 4.46

png(filename="plots/SR-4.46-posterior-mean-values.png")
plot( height ~ weight , data=d2 , col=rangi2 )
post <- extract.samples( m4.3 )
a_map <- mean(post$a)
b_map <- mean(post$b)
curve( a_map + b_map*(x - xbar) , add=TRUE )
dev.off()

# Rcode 4.47
post <- extract.samples(m4.3)
post[1:5,]

# Rcode  4.48
N <- 10
dN <- d2[ 1:N , ]
mN <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*( weight - mean(weight) ) ,
    a ~ dnorm( 178 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=dN )

# Rcode 4.49

post <- extract.samples( mN , n=20 )

png(filename="plots/SR-4.49-quadratic-approximate-posterior-distribution-height-weight-model.png")

plot( dN$weight , dN$height ,
      xlim=range(d2$weight) , ylim=range(d2$height) ,
      col=rangi2 , xlab="weight" , ylab="height" )
mtext(concat("N = ",N))

for ( i in 1:20 )
  curve( post$a[i] + post$b[i]*(x-mean(dN$weight)) ,
         col=col.alpha("black",0.3) , add=TRUE )

dev.off()


# Rcode 4.50

post <- extract.samples(m4.3)

mu_at_50 <- post$a + post$b * (50-xbar)
write.csv(mu_at_50, "tables/4.50_mu_at_fifty.csv")

# Rcode 4.51

png(filename="plots/SR-4.51-mu-at-fifty.png")
dens( mu_at_50 , col=rangi2 , lwd=2 , xlab="mu|weight=50" )
dev.off()

# Rcode 4.52

PI(mu_at_50, prob=0.89)

# Rcode 4.53
mu <- link(m4.3)
str(mu)

# Rcode 4.54
weight.seq <- seq(from=25, to=70, by=1)
mu <- link( m4.3 , data=data.frame(weight=weight.seq) )
str(mu)

# Rcode 4.55

plot( height ~ weight , d2 , type="n" )
for ( i in 1:100 )
  points( weight.seq , mu[i,] , pch=16 , col=col.alpha(rangi2,0.1) )

# Rcode 4.56
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )

# Rcode 4.57

png(filename="plots/SR-4.57-89pc-interval-of-the-mean-height-weight.png")
plot( height ~ weight , data=d2 , col=col.alpha(rangi2,0.5) )
lines( weight.seq , mu.mean )
shade( mu.PI , weight.seq )
dev.off()


# Rcode 4.59

sim.height <- sim(m4.3, data=list(weight=weight.seq))
str(sim.height)
write.csv(sim.height, "tables/4.59_sim_height.csv")

# Rcode 4.60

height.PI <- apply(sim.height, 2, PI, prob=0.89)

# Rcode 4.61

plot( height ~ weight , d2 , col=col.alpha(rangi2,0.5) )
lines( weight.seq , mu.mean )
shade( mu.PI , weight.seq ) # Book says mu.HPDI
shade( height.PI , weight.seq )

# Rcode 4.63

post <- extract.samples(m4.3)
weight.seq <- 25:70
sim.height <- sapply( weight.seq , function(weight)
  rnorm(
    n=nrow(post) ,
    mean=post$a + post$b*( weight - xbar ) ,
    sd=post$sigma ) )
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

## 4.5 Curves from lines ##

# Rcode 4.64

library(rethinking)
data(Howell1)
d <- Howell1

png(filename="plots/SR-4.64-height-weight-all-ages.png")
plot(height ~ weight, d)
dev.off()

# Rcode 4.65

d$weight_s <- ( d$weight - mean(d$weight) )/sd(d$weight)
d$weight_s2 <- d$weight_s^2
m4.5 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*weight_s + b2*weight_s2 ,
    a ~ dnorm( 178 , 20 ) ,
    b1 ~ dlnorm( 0 , 1 ) ,
    b2 ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=d )

# Rcode 4.66
precis(m4.5)

# Rcode 4.67

weight.seq <- seq( from=-2.2 , to=2 , length.out=30 )
pred_dat <- list( weight_s=weight.seq , weight_s2=weight.seq^2 )
mu <- link( m4.5 , data=pred_dat )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )
sim.height <- sim( m4.5 , data=pred_dat )
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

# Rcode 4.68

png(filename="plots/SR-4.68-polynomial-regressions.png")
plot( height ~ weight_s , d , col=col.alpha(rangi2,0.5) )
lines( weight.seq , mu.mean )
shade( mu.PI , weight.seq )
shade( height.PI , weight.seq )
dev.off()

# Rcode 4.69 # Does not work
d$weight_s3 <- d$weight_s^3
m4.6 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*weight_s + b2*weight_s2 + b3*weight_s3 ,
    a ~ dnorm( 178 , 20 ) ,
    b1 ~ dlnorm( 0 , 1 ) ,
    b2 ~ dnorm( 0 , 10 ) ,
    b3 ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=d )

weight.seq <- seq( from=-2.2 , to=2 , length.out=30 )
pred_dat <- list( weight_s=weight.seq , weight_s2=weight.seq^2, d$weight_s3 <- d$weight_s^3 )
mu <- link( m4.6 , data=pred_dat ) # Error in eval(parse(text = lm), envir = e) : object 'weight_s3' not found
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )
sim.height <- sim( m4.6 , data=pred_dat )
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

plot( height ~ weight_s , d , col=col.alpha(rangi2,0.5) )
lines( weight.seq , mu.mean )
shade( mu.PI , weight.seq )
shade( height.PI , weight.seq )

# Splines

# Rcode 4.72

library(rethinking)
data(cherry_blossoms)
d <- cherry_blossoms
precis(d)

#p.115
library(ggplot2)
ggplot(d, aes(x=year, y=doy)) + geom_point() + scale_x_continuous(name ="Year") + scale_y_continuous(name ="Day of the Year")
ggsave("PLOTS/SR-4.72-Hanami.png", plot = last_plot(), device = png(), units="cm", width=30, height=20, scale=1)

# Rcode 4.73
d2 <- d[complete.cases(d$doy), ]
num_knots <- 15
knot_list <- quantile(d2$year, probs=seq(0, 1, length.out=num_knots))
write.csv(knot_list, "tables/4.73_knot_list.csv")

# Rcode 4.74

library(splines)
B <- bs(d2$year, knots=knot_list[-c(1, num_knots)], degree=3, intercept=TRUE)
write.csv(B, "tables/4.74_polynomial_degree.csv")

# Rcode 4.75

png(filename="plots/SR-4.75-year-vs-basis-value.png")
plot( NULL , xlim=range(d2$year) , ylim=c(0,1) , xlab="year" , ylab="basis value" )
for ( i in 1:ncol(B) ) lines( d2$year , B[,i] )
dev.off()

# Rcode 4.76

m4.7 <- quap(
  alist(
    T ~ dnorm( mu , sigma ) ,
    mu <- a + B %*% w ,
    a ~ dnorm(6,10),
    w ~ dnorm(0,1),
    sigma ~ dexp(1)
  ),
  data=list( T=d2$doy , B=B ) ,
  start=list( w=rep( 0 , ncol(B) ) ) )

precis(m4.7, depth=2)
