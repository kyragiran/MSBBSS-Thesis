library(MASS)  
library(mice) 
library(Matrix)


set.seed(123)  
n <- 500
p <- 49 


#correlation matrix 
rho <- 0.5  # correlation coefficient
Sigma <- matrix(rho, nrow = p, ncol = p)
diag(Sigma) <- 1




# Generate multivariate normal data
mu <- rep(0, p)  # mean vector of zeroes
data <- mvrnorm(n = n, mu = mu, Sigma = Sigma)

cor(data)
# Convert to a dataframe
df <- as.data.frame(data)

df$y <- c(as.matrix(data) %*% c(rep(0.2, 4), rep(0, p-4)) + rnorm(n, 0, 1))

summary(lm(y~., df))

show(data)