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

