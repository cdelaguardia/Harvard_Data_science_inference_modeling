#library(tidyverse)
#options(digits = 3)

# load brexit_polls object
#library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

#Consider a poll with a sample of 𝑁=1500 voters.

#What is the expected total number of voters in the sample choosing “Remain”?
N <- 1500
print(expected <- N*p)

#What is the standard error of the total number of voters in the sample 
#choosing “Remain”?

print(stand_error <- sqrt(N*p*(1-p)))

#What is the expected value of 𝑋̂ , the proportion of “Remain” voters?
print(x_hat <- p)

#What is the standard error of 𝑋̂ , the proportion of “Remain” voters?
print(stand_error_x_hat <- sqrt(x_hat*(1 - x_hat)/N))

#What is the expected value of 𝑑 , the spread between the proportion 
#of “Remain” voters and “Leave” voters?
print(d <- 2*p - 1)

#What is the standard error of 𝑑 , the spread between the proportion 
#of “Remain” voters and “Leave” voters?
print(stand_error_d <- 2*stand_error_x_hat)

#What is the average of the observed spreads (spread)?
brexit_poll_hat <- brexit_polls %>% mutate(x_hat = (spread + 1)/2)
mean(brexit_poll_hat$spread)

#What is the standard deviation of the observed spreads?
sd(brexit_poll_hat$spread)

#What is the average of x_hat, the estimates of the parameter
mean(brexit_poll_hat$x_hat)

#What is the standard deviation of x_hat?
sd(brexit_poll_hat$x_hat)

#Use qnorm() to compute the 95% confidence interval for 𝑋̂.
#What is the lower bound of the 95% confidence interval?
N <- brexit_poll_hat$samplesize[1]
stand_error_x_hat <- sqrt(brexit_poll_hat$x_hat[1]*(1 - brexit_poll_hat$x_hat[1])/N)
conf_interval_x_hat <- c(brexit_poll_hat$x_hat[1] - qnorm(0.975)*stand_error_x_hat, brexit_poll_hat$x_hat[1] + qnorm(0.975)*stand_error_x_hat)

#How many polls are in june_polls?
june_polls <- brexit_poll_hat %>% filter(enddate >= '2016-06-01')
length(june_polls$enddate)

#What proportion of polls have a confidence interval that covers the value 0?
d <- -0.038
june_polls <- june_polls %>% mutate(se_x_hat = sqrt(x_hat*(1 - x_hat)/samplesize),
  se_spread = 2*se_x_hat,
  lower = spread - se_spread*qnorm(0.975),
  upper = spread + se_spread*qnorm(0.975),
  hit = (lower < d & upper > d))
mean(june_polls$lower <= 0 & june_polls$upper >= 0)

#What proportion of polls have a confidence interval covering the true value of 𝑑 ?
mean(june_polls$hit)

