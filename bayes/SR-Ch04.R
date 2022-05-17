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
