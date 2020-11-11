#####################
##  Random numbers ##
##    and          ##
##  Functions      ##
#####################

# Clear memory and load packages
rm(list=ls())
library(tidyverse)

#######
# Random numbers
#   Random numbers are often used in data science:
#     - get a random (sub)-sample
#     - bootstrapping
#     - other 'stochastic' optimization or 
#     - in some estimation (typically with ML)
#

# Keys for random numbers:
#   1) Theoretical distribution - from what kind of distribution should it sample
#       a) Actually it is a hard problem computationally 
#           -> methods implemented are pretty good, but not perfect (pseudo random methods)
#   2) Reproducible
#       a) If you estimate something which uses random numbers you should allow for the 'key'
#       b) In some cases this does not matter, the result 'averages out'
#
# Great opportunity for short presentation!



# 1) case uniform distribution random sampling
# random value from uniform distribution
# unif -uniform

n <- 10
x <- runif(n, min = 0, max = 10)
x

# set seed for the random number generation (in order to see same numbers every time)
# Set the seed for the computer for rng

set.seed(123)
x <- runif(n, min = 0, max = 10)
x
# if you run these three lines, every time it generates same numbers

#what does 123 in seed means? to make your code reproducible...

#if you have for loop, and if seed is outside, every time you see different numbers
# if seed is inside loop, every time you see same numbers

#Play around with no

#rnorm - random normal

n<- 10
y <- rnorm(n, mean = 1, sd = 2)
df <- tibble(var1 = y)
ggplot(df, aes(x = var1)) + 
  geom_histogram(aes(y = ..density..), fill = "navyblue") + 
  stat_function(fun = dnorm, args = list(mean = 1, sd = 2), 
                color ='red', size = 1.5)
  
  
#how is our sample we made from distribution tend to be same as hypothetical normal distribution

# with more sample, closer 
# now histogram is better

n<- 10000
y <- rnorm(n, mean = 1, sd = 2)
df <- tibble(var1 = y)
ggplot(df, aes(x = var1)) + 
  geom_histogram(aes(y = ..density..), fill = "navyblue") + 
  stat_function(fun = dnorm, args = list(mean = 1, sd = 2), 
                color ='red', size = 1.5)

# There are some other type of distribution
# rbinom,rexp, rlnorm, etc.

?rnorm


# Random sampling from a dataset/variable:
# x is dataset, or vector, or dataframe

# sample_1 is without replacement
#representative sample of y
sample_1 <- sample(y, 1000, replace = FALSE)


# sample_2 with replacement -> useful for bootstraping

sample_2 <- sample(y, 1000, replace = TRUE)

# you are just taking same observations because this equals to original dataset
sample_1 <- sample(y, 10000, replace = FALSE)

# you cant have more observations than original dataset
sample_1 <- sample(y, 10000, replace = FALSE)


############
## FUNDTIONS
############

#1) simplest case - calculate the mean
# supply input of a function - x
# do some manipulation with x and speed up y
my_fun <- function(x){
  sum_x <- sum(x)
  sum_x / length(x)
}

#Use this function
# gives sample mean of y
my_fun(y)

# You can save the output of the function
mean_y <- my_fun(y)

# 2) Calculate mean and sd
my_fun2 <- function(x){
  sum_x <- sum(x)
  sum_x / length(x)
  sd_x <- sd(x)
}

# R always returns you last expression (even though you dont defince any return statement)

# Check the output 
what_y <- my_fun2(y)
what_y

#3) I want to control my output

my_fun3 <- function(x){
  sum_x <- sum(x)
  mean_x <- sum_x / length(x)
  sd_x <- sd(x)
  return(mean_x)
}

my_fun3(y)

# 4) Multiple outputs

#3) I want to control my output
# for multiple outputs its better to use list

my_fun4 <- function(x){
  sum_x <- sum(x)
  mean_x <- sum_x / length(x)
  sd_x <- sd(x)
  output <- list("sum" = sum_x, "mean" = mean_x, "sd" = sd_x)
  return(output)
}

my_fun4(y)


my_fun_no_return <- function(x){
  sum_x <- sum(x)
  sum_x / length(x)
  sd_x <- sd(x)
  a <- 5
}

my_fun_no_return(y)
result_demo <- my_fun_no_return(y)

#5) Multiple input
#CI - confidence interval
# we want to calculate confidence interval of mean
# what input we need for the function 

#use missing value rm.no - if x includes some missing values, drop them from calculation

my_CI_fun <- function(x, CI = 0.95){
  # mean of x
  mean_x <- mean(x, na.rm = TRUE)
  # SD 
  sd_x <- sd(x, na.rm = TRUE)
  # Calculate the number of observations in x
  n_x <- sum(!is.na( x ))
  # Calculate the theoretical standard error for mean of x 
  se_mean_x <- sd_x / sqrt(n_x)
  #Calculate the CI
  if(CI == 0.95){
    #lower and upper bound
    CI_mean <- c(mean_x - 2*se_mean_x, mean_x + 2*se_mean_x)
  } else if(CI == 0.99){
    CI_mean <- c(mean_x - 2.6*se_mean_x, mean_x + 2.6*se_mean_x)
  } else {
    stop("No such CI implemented, use 0.95 or 0.99")
  }
  output <- list("mean" = mean_x, "CI_mean" = CI_mean)
  
}

# t, degree of freedom exists in R but this was the way we can create our own function

#Get some CI values
my_CI_fun(y, CI = 0.95)

result_demo <- my_CI_fun(y, CI = 0.95)

#if you assigned a value to parameter when you declared a function, no need to supply it as an argument when you call a function


# how to define type of variable

# can we do na.rm for length


# Sampling distribution - try to do this on your own #practice #independent #try #donotgiveup
# uniform distribution

set.seed((100))
X <- runif(10000, min = 0, max = 2)

#function
#for loop
# in each repetition

# i is a sample


# Write a function which creates the sampling distribution for:
# - for the mean (mu)
# - 1st t-statistics where H0: mu = 1((mu - 1) /SE(mu))
# - 2nd t-statistics where H0: mu = 0(mu /SE(mu))

# In the function you should add inputs as:
  # x - vector
  # rep_num <- how many time it should sample
  # sample_size - how many observations to sample from x

# Notes: use sample(), use tibble as output, use for loop
  #initialize vectors for the loop

get_sampling_dists <- function(x, rep_num = 1000, sample_size = 1000){
  #Check inputs 
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(rep_num), length(rep_num) == 1, rep_num > 0)
  stopifnot(is.numeric(sample_size), length(sample_size) == 1, 
            sample_size > 0, sample_size <= length(x) )
  
  # Initialize the for loop
  # in each repetition, we want differerent sample, thats why seed is before loop
  set.seed(100)
  mean_stat <- double(rep_num)
  t_stat_1 <- double(rep_num)
  t_stat_2 <- double(rep_num)
  #Usual scalar for SE
  sqrt_n <- sqrt(sample_size)
  for(i in 1:rep_num){
    #Need a new sample
    # you can add prob arg also: probability
    sample_i <- sample(x, sample_size, replace = FALSE)
    #Save computed statistics
    # Mean for sample_i
    mean_stat[i] <- mean(sample_i)
    # SE for Mean
    se_mean <- sd(sample_i) / sqrt_n
    #T statistics for hypothesis
    t_stat_1[i] <- (mean_stat[i] - 1/ se_mean)
    t_stat_2[i] <- mean_stat[i]/ se_mean
  }
  
  df <- tibble(mean_stat = mean_stat, t_stat_1 = t_stat_1, 
               t_stat_2 = t_stat_2)
  
  
}

# Get some sampling distribution

df_X <- get_sampling_dists(X, rep_num = 1000, sample_size = 1000)


# how to define an error message with stopifnot

# Plot these distribution
ggplot(df_X, aes(x = mean_stat)) + 
  geom_density(color = 'red') + 
  geom_vline(xintercept = 1, linetype = 'dashed', color = 'blue') + 
  geom_vline(xintercept = mean(X), color = 'green') 
  

  
# What is the probability of having a mean value that is equal to true mean
# It is highly likely
  

# How sure are you - is given by SE of mean  
  
  
ggplot(df_X, aes(x =t_stat_1)) + 
  geom_density(color = 'red') 

# Plot these distributions - H0 is not true: mean is zero

ggplot(df_X, aes(x =t_stat_2)) + 
  geom_density(color = 'red') 

