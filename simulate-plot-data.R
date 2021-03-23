#simulate and plot data
# Erin Tracy 
# etracy1121@gmail.com
# 2021-03-23

#Simulate data from normal distribution
x <- rnorm(n = 100, mean=6)
y <- 3 * x + rnorm(n = 100, sd= 0.2)

#plot simulated data
plot(x = x, y = y, main = "Simulated Data")

#save file, add by clicking box in github tab, 
#click commit, write a commit message and commit, push (green arrow) to github
