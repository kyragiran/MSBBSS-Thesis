# 1_data_generation.R

library(MASS)
library(mice)
library(Matrix)

set.seed(123)
n <- 500
p <- 49

# Create correlation matrix
rho <- 0.5
Sigma <- matrix(rho, nrow = p, ncol = p)
diag(Sigma) <- 1

# Generate multivariate normal predictors
mu <- rep(0, p)
data <- mvrnorm(n = n, mu = mu, Sigma = Sigma)
df <- as.data.frame(data)

# Generate outcome variable
df$y <- as.numeric(as.matrix(df) %*% c(rep(0.2, 4), rep(0, p - 4)) + rnorm(n, 0, 1))

# Function to create missing data (kept for flexibility)
create_missing_data <- function(data, prop, mech) {
  ampute(data, prop = prop, mech = mech)$amp
}

# Generate MCAR and MAR datasets at different levels
missing_props <- c(0.1, 0.25, 0.5)
missing_datasets <- list()

for (prop in missing_props) {
  missing_datasets[[paste0("MCAR_", prop * 100, "%")]] <- create_missing_data(df, prop, "MCAR")
  missing_datasets[[paste0("MAR_", prop * 100, "%")]] <- create_missing_data(df, prop, "MAR")
}

# Save full data and missing scenarios
saveRDS(df, "original_data.RDS")
saveRDS(missing_datasets, "missing_datasets.RDS")
