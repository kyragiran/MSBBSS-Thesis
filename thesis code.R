
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

amputed_data <- ampute(df, prop = 0.5, mech = "MCAR")$amp  # 50% missing, MCAR

summary(amputed_data)

md.pattern(amputed_data)

is.na(data) 
is.na(amputed_data)

colMeans(data)
colMeans(amputed_data)


#MISSING DATA IMPUTATION -> MEAN

imp <- mice(amputed_data, method = "mean", m = 1, maxit = 1)


imputed_data <- complete(imp) 
#for multiple imputation dont use complete -> 

summary(amputed_data)  # Before
summary(imputed_data)  # After

#REGRESSION MODEL

model_imp <- lm(y ~ V1 + V2 + V3 + V4, data = imputed_data)

#model_og <- lm(y ~ V1 + V2 + V3 + V4, data = data)

summary(model_imp)
#summary(model_og)


library(Metrics)
install.packages("Metrics")
# Get predicted values

#
coef(model_imp)

confidence_interval_imp = confint(model_imp)
class(confidence_interval_imp)



predictions <- predict(model, newdata = imputed_data)

# Compute RMSE
# LOOK UP FURTHER which RMSE
rmse_value <- rmse(imputed_data$V1, predictions)
print(paste("RMSE:", rmse_value))

# Compute bias
bias_value <- coef(model_imp) - c(0, rep(0.2, 4))
print(paste("Bias:", bias_value))

# Extract standard errors and compute 95% CI

coef(model_imp)

confidence_interval_imp = confint(model_imp)
class(confidence_interval_imp)

confidence_interval_imp[-1, 1] #lower bounds
confidence_interval_imp[-1, 2] #upper bounds

confidence_interval_imp[-1, 1] < 0.2 & confidence_interval_imp[-1, 2] > 0.2 #determining if the confidence intervals are correct
  

#TO DO
#check which RMSE -> literature
#implement the RMSE

#implement multiple imputation -> calc regression coef on all dataset with pool()
#how to implement VAEs
#finalize a schedule

# focus should be predictive RMSE


library(MASS)  
library(mice) 
library(Matrix)
library(Metrics)

set.seed(123)  
n <- 500
p <- 49 

# Define correlation matrix
rho <- 0.5  
Sigma <- matrix(rho, nrow = p, ncol = p)
diag(Sigma) <- 1

# Generate multivariate normal data
mu <- rep(0, p)  
data <- mvrnorm(n = n, mu = mu, Sigma = Sigma)

# Convert to a dataframe
df <- as.data.frame(data)

df$y <- c(as.matrix(data) %*% c(rep(0.2, 4), rep(0, p-4)) + rnorm(n, 0, 1))

# Introduce missingness (50% MCAR)
amputed_data <- ampute(df, prop = 0.5, mech = "MCAR")$amp  

# Multiple imputation
m <- 5  # Number of imputations
imp <- mice(amputed_data, method = "pmm", m = m, maxit = 5)

# Perform regression on each imputed dataset and pool results
model_imp <- with(imp, lm(y ~ V1 + V2 + V3 + V4))
pooled_model <- pool(model_imp)

# Display summary of pooled results
summary(pooled_model)

# Compute pooled RMSE
predictions <- complete(imp, "long")  # Get all imputed datasets in long format
predictions$predicted <- predict(model_imp$analyses[[1]], newdata = predictions)

# Compute RMSE across all imputations
rmse_values <- sapply(model_imp$analyses, function(mod) {
  preds <- predict(mod, newdata = complete(imp, "long"))
  rmse(complete(imp, "long")$V1, preds)
})

mean_rmse <- mean(rmse_values)
print(paste("Pooled RMSE:", mean_rmse))

# Compute pooled bias
pooled_coefs <- summary(pooled_model)$estimate
bias_value <- pooled_coefs - c(0, rep(0.2, 4))
print(paste("Pooled Bias:", bias_value))

print(pooled_coefs)


# Extract confidence intervals

confidence_intervals <- summary(pooled_model, conf.int = TRUE)
print(confidence_intervals)

confidence_int_bounds <- confidence_intervals[, c("2.5 %", "97.5 %")]
ci_check <- confidence_int_bounds[-1, 1] < 0.2 & confidence_int_bounds[-1, 2] > 0.2
print(ci_check)


# make a matrix complete with either method
# and then we compare what mice does
# try to get a complete dataset for the boys dataset from mice
# or just use VAE on a complete dataset -> check correlation between variable, density plots of the variables
#look at papaers that did VAES on missing data, what do they do
#1. apply on a complete dataset
#2. what other people do and how -> papers with code
#3. try it with missing data 


#INTRODUCTION:
# research report-> improve based on the feedback, if something is missing write a bullet
# halway march: methods+intro done
# 26th feb -> whole code
# VAEs trial next week
#VAEs with incomplete data -> focus on generate imputations with a VAE
# before running it we should test it out with smaller trial runs