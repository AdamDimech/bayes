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

# Grid approximation

G=10