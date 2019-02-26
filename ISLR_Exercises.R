# Session 4: Resampling
# Lab 5.3
# (1) The Validation Set Approach
library(ISLR)
data(Auto)

# Use set.seed() function to set a seed for R's random number generator 
# For replication 
set.seed(1)
# Use sample() to split set of observations into two halves 
train = sample(392,196)
# Use subset option in lm() to fit linear regression using only training set
lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)
# Use predict() function to estimate response for all 392 observations 
# Use mean() function to calculate the MSE of the 196 observations in validation set 
# -train index selects only observations NOT in the training set 
attach(Auto)
mean((mpg-predict(lm.fit, Auto))[-train]^2)
# Estimated test MSE for linear reg fit is 26.14
# Estimate test error for quadratic and cubic regressions
lm.fit2 = lm(mpg ~ poly(horsepower,2), data = Auto, subset = train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)
# Test error rate = 19.82
lm.fit3 = lm(mpg ~ poly(horsepower,3), data = Auto, subset = train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)
# Test error rate = 19.78

# Use a different training set 
set.seed(2)
# Use sample() to split set of observations into two halves 
train = sample(392,196)
# Use subset option in lm() to fit linear regression using only training set
lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)
# Use predict() function to estimate response for all 392 observations 
# Use mean() function to calculate the MSE of the 196 observations in validation set 
# -train index selects only observations NOT in the training set 
attach(Auto)
mean((mpg-predict(lm.fit, Auto))[-train]^2)
# Estimated test MSE for linear reg fit is 23.30
# Estimate test error for quadratic and cubic regressions
lm.fit2 = lm(mpg ~ poly(horsepower,2), data = Auto, subset = train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)
# Test error rate = 18.90
lm.fit3 = lm(mpg ~ poly(horsepower,3), data = Auto, subset = train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)
# Test error rate = 19.26
# A model that predicts mpg using a quadratic function of `horsepower` performs
# better than a model that involves only a linear function 

# (2) Leave-One-Out Cross-Validation (LOOCV)
# Can be automatically computed for any generalised linear model 
glm.fit = glm(mpg ~ horsepower, data = Auto)
coef(glm.fit)
lm.fit = lm(mpg ~ horsepower, data = Auto)
coef(lm.fit)
# GLM and linear regression yield similar results
# We can use glm() to perform linear regression so that we can use cv.glm()
library(boot)
glm.fit = glm(mpg ~ horsepower, data = Auto)
cv.err = cv.glm(Auto, glm.fit)
cv.err$delta
# Results: [1] 24.23151 24.23114
# cv.glm() function produces a list - numbers in the delta vector contain the 
# cross-validation results 
# Results are identical in this case. 
# Cross-validation estimate for the test error is approx. 24.23

# Repeat for other more complex polynomial fits - using for loop 
# For polynomials of order 1 to 5. the loop computes associated CV error
# and stores it in the ith element of the vector cv.error 
cv.error = rep(0,5)
for(i in 1:5){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}
cv.error
# Results: [1] 24.23151 19.24821 19.33498 19.42443 19.03321
# Test MSE decreases from linear to quadratic fit but no 
# clear improvement from using higher-order polynomials 

# (3) k-Fold Cross-Validation
# Set random seed
set.seed(17)
# Create vector to store CV errors corresponding to polynomial fits of orders one to ten 
cv.error.10 = rep(0,10)
for(i in 1:10){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error.10
# Results:  [1] 24.20520 19.18924 19.30662 19.33799 18.87911 19.02103 
#18.89609 19.71201 
# [9] 18.95140 19.50196
# Estimates are quite similar to LOOCV and takes a shorter time to process

# (4) Bootstrap 
data(Portfolio)
# Step 1: Create a function that computes the statistic of interest 
# Use the alpha function [alpha.fn()] 
# Input for the function includes (X, Y) data and vector indicating which 
# observations should be used to estimate alpha. 
# Output is the estimate for alpha (the value to minimise the variance/risk) 
# based on the selected observations 
alpha.fn = function(data, index){
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y) - cov(X, Y))/(var(X) + var(Y) - 2*cov(X, Y)))
}
# Estimate alpha using all 100 observations 
alpha.fn(Portfolio, 1:100)
# alpha = 0.5758321 

# Step 2: Use the boot() function to perform the bootstrap by repeatedly sampling 
# observations from the data set with replacement
# Manual approach: 
# Use sample() function to randomly select 100 observations with replacement 
set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = TRUE))
# alpha = 0.5963833
# repeat many times and record all corresponding estimates for alpha 

# Using boot() function 
# Produce R = 1,000 bootstrap estimates for alpha 
boot(Portfolio, alpha.fn, R = 1000)
# alpha-hat = 0.5758321 
# Standard error for alpha-hat = 0.08861826

# Bootstrap method can be used to assess variability of coefficient estimates 
# Step 1: Create boot.fn(), a function that uses the Auto dataset 
# and a set of indices for the observations and returns the intercept and slope 
# estimates for the linear regression model 
attach(Auto)
boot.fn = function(data, index)
return(coef(lm(mpg ~ horsepower, data = data, subset = index)))

# Step 2: Apply function to full set of 392 observations to compute estimates 
# of intercept and slope using linear regression 
boot.fn(Auto, 1:392)

# (Intercept)  horsepower 
# 39.9358610  -0.1578447 

# Step 3: Use boot.fn() function to create bootstrap estimates for intercept and
# slope terms by randomly sampling from among observations with replacement 
set.seed(1)
boot.fn(Auto, sample(392, 392, replace = TRUE))

# (Intercept)  horsepower 
# 38.7387134  -0.1481952 
# This can be repeated several times - but we automate this with boot()

# Step 4: Use boot() function to compute standard errors of 1000 bootstrap estimates 
boot(Auto, boot.fn, 1000)

# Estimate for standard error of intercept = 0.86
# Estimate for standard error of slope = 0.0074

# Obtaining these estimates with the summary() of the lm formula: 
summary(lm(mpg ~ horsepower, data = Auto))$coef
# Estimate for s.e. of intercept = 0.717
# Estimate for s.e. of slope = 0.0064
# This formula relies on linear regression being the correct specification 
# Quadratic formula may be a better approximation of r/ship of data 

# Bootstrap approach does not rely on such assumptions - it is likely giving 
# a more accurate estimate of the s.e. 