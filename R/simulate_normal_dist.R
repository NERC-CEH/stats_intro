# R code to demonstrate the Normal distribution
# Peter Levy
# CEH Edinburgh
# 26/01/2024
library(ggplot2)

# We are going to simulate how the Normal distribution comes about 
# and what it means, by simulating random variation.

# 1. Take the average of some random values from the uniform distribution
# Let's look at what the uniform distribution looks like:
# All numbers in the range 100 to 200 are equally likely.
x <- runif(n = 10000, min = 100, max = 200)
hist(x)

# function to add n uniform random_numbers between x_lower and x_upper
average_random_numbers <- function(n_numbers, x_lower = 100, x_upper = 200) {
  random_numbers <- runif(n_numbers, x_lower, x_upper)
  return(mean(random_numbers))
}

# To illustrate, we can take three random numbers and take their mean: 
average_random_numbers(n_numbers = 3, x_lower = 100, x_upper = 200)
# which should be different every time
average_random_numbers(n_numbers = 3, x_lower = 100, x_upper = 200)
average_random_numbers(n_numbers = 3, x_lower = 100, x_upper = 200)

# And we can simulate what happens if we repeat this many times (simulations, n_sims) 
n_sims <- 10000
n_numbers <- 3
# add n_numbers n_sims times
x <- sapply(rep(n_numbers, n_sims), average_random_numbers)

# We can plot the 10000 averages as a histogram
p <- ggplot(data = data.frame(x), aes(x))
p <- p + geom_histogram(aes(y =..density..), colour = "black", fill = "white")
# and compare with the Normal distribution centred on the mean of the simulated data
p <- p + stat_function(fun = dnorm, args = list(mean = mean(x), sd = sd(x)))
p


# 2. This works for any distribution. Next we do the same with the average of 
# random values from a skewed distribution. We use the beta distribution as an example.
# See https://en.wikipedia.org/wiki/Beta_distribution for examples.
# 
# left-skewed random numbers in range 0 to 1
x <- rbeta(n = 10000, 2, 5)
# rescale to range x_lower to x_upper
x <- x * (x_upper - x_lower) + x_lower
hist(x)

# function to add n skewed random_numbers between x_lower and x_upper
average_skewed_random_numbers <- function(n_numbers, x_lower = 100, x_upper = 200) {
  random_numbers <- rbeta(n_numbers, x_lower, x_upper)
  # left-skewed random numbers 0-1
  random_numbers <- rbeta(n_numbers, 2, 5)
  # rescale to range x_lower to x_upper
  random_numbers <- random_numbers * (x_upper - x_lower) + x_lower
  return(mean(random_numbers))
}

# So we take three random numbers and take their mean: 
average_skewed_random_numbers(n_numbers = 3, x_lower = 100, x_upper = 200)
# which should be different every time
average_skewed_random_numbers(n_numbers = 3, x_lower = 100, x_upper = 200)
average_skewed_random_numbers(n_numbers = 3, x_lower = 100, x_upper = 200)

# And we can simulate what happens if we repeat this many times (simulations, n_sims) 
x <- sapply(rep(n_numbers, n_sims), average_skewed_random_numbers)

# and compare the histogram with the Normal distribution as before
p <- ggplot(data = data.frame(x), aes(x))
p <- p + geom_histogram(aes(y =..density..), colour = "black", fill = "white")
p <- p + stat_function(fun = dnorm, args = list(mean = mean(x), sd = sd(x)))
p

# Try repeating with higher and lower values of n_numbers and n_sims, and different 
# skewness parameters. e.g. n_numbers <- 10


# 3. What if we combine random numbers from different distribution?
# To investigate, we simulate from both uniform and skewed distributions and average them.
n_sims <- 10000
n_numbers <- 3
# add n uniform numbers n_sims times
x_unif <- sapply(rep(n_numbers, n_sims), average_random_numbers)
# add n skewed  numbers n_sims times
x_skew <- sapply(rep(n_numbers, n_sims), average_skewed_random_numbers)
# and average over the two:
x <- (x_unif + x_skew) / 2

# and compare the histogram with the Normal distribution as before
p <- ggplot(data = data.frame(x), aes(x))
p <- p + geom_histogram(aes(y =..density..), colour = "black", fill = "white")
p <- p + stat_function(fun = dnorm, args = list(mean = mean(x), sd = sd(x)))
p

# 4. Lastly, what if we multiply the random numbers instead of averaging or adding them?

# function to add n uniform random_numbers between x_lower and x_upper
multiply_random_numbers <- function(n_numbers, x_lower = 1, x_upper = 10) {
  random_numbers <- runif(n_numbers, x_lower, x_upper)
  return(prod(random_numbers))
}

# So we take three random numbers and take their product: 
multiply_random_numbers(n_numbers = 3, x_lower = 1, x_upper = 10)
# which should be different every time                        
multiply_random_numbers(n_numbers = 3, x_lower = 1, x_upper = 10)
multiply_random_numbers(n_numbers = 3, x_lower = 1, x_upper = 10)

# And we can simulate what happens if we repeat this many times (simulations, n_sims) 
n_sims <- 10000
n_numbers <- 3
# add n_numbers n_sims times
x <- sapply(rep(n_numbers, n_sims), multiply_random_numbers)

# We can plot the 10000 averages as a histogram
p <- ggplot(data = data.frame(x), aes(x))
p <- p + geom_histogram(aes(y =..density..), colour = "black", fill = "white")
# and compare with the Normal distribution centred on the mean of the simulated data
p <- p + stat_function(fun = dnorm, args = list(mean = mean(x), sd = sd(x)))
p
# This gives a skewed distribution very different from the Normal one.
# But logarithms convert multiplication into addition, so the lognormal distribution 
# gives the corresponding match.
p <- p + stat_function(fun = dlnorm, args = list(mean = mean(log(x)), sd = sd(log(x))), colour = "red")
p