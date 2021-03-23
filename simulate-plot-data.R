#simulate and plot data
# Erin Tracy 
# etracy1121@gmail.com
# 2021-03-23

#Simulate data from normal distribution
x <- rnorm(n = 100)
y <- 2 * x + rnorm(n = 100, sd= 0.1)

#plot simulated data
plot(x = x, y = y)
