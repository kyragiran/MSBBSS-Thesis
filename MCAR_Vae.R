library(torch)
library(dplyr)
library(ggplot2)
library(Metrics)


#MCAR missingness

train_vae_and_impute <- function(data, m = 5, epochs = 100, latent_dim = 2, lr = 0.001) {
  
  data_matrix <- as.matrix(data)
  
  # Create a missing mask (1 = observed, 0 = missing)
  missing_mask <- ifelse(is.na(data_matrix), 0, 1)
  
  # Replace NA with zeros
  data_matrix[is.na(data_matrix)] <- 0
  
  # Convert to torch tensors
  data_tensor <- torch_tensor(data_matrix, dtype = torch_float())
  mask_tensor <- torch_tensor(missing_mask, dtype = torch_float())
  
  # Encoder module
  encoder_module <- nn_module(
    "encoder_module",
    initialize = function(in_features, latent_dim) {
      self$model <- nn_sequential(
        nn_linear(in_features, 64),
        nn_relu(),
        nn_linear(64, latent_dim * 2)
      )
      self$latent_dim <- latent_dim
    },
    forward = function(x) {
      out <- self$model(x)
      mu <- out[, 1:self$latent_dim]
      logvar <- out[, (self$latent_dim + 1):(2 * self$latent_dim)]
      list(mu, logvar)
    }
  )
  
  # Decoder module
  decoder_module <- nn_module(
    "decoder_module",
    initialize = function(latent_dim, out_features) {
      self$model <- nn_sequential(
        nn_linear(latent_dim, 64),
        nn_relu(),
        nn_linear(64, out_features)
      )
    },
    forward = function(z) {
      self$model(z)
    }
  )
  
  # VAE model combining encoder and decoder
  vae_module <- nn_module(
    "vae_module",
    initialize = function(in_features, latent_dim) {
      self$encoder <- encoder_module(in_features, latent_dim)
      self$decoder <- decoder_module(latent_dim, in_features)
      self$latent_dim <- latent_dim
    },
    reparameterize = function(mu, logvar) {
      std <- torch_exp(0.5 * logvar)
      eps <- torch_randn_like(std)
      mu + eps * std
    },
    forward = function(x) {
      enc_out <- self$encoder(x)
      mu <- enc_out[[1]]
      logvar <- enc_out[[2]]
      z <- self$reparameterize(mu, logvar)
      reconstruction <- self$decoder(z)
      list(reconstruction, mu, logvar)
    }
  )
  
  # Loss function with missing data handling
  vae_loss <- function(output_list, input, mask) {
    reconstruction <- output_list[[1]]
    mu <- output_list[[2]]
    logvar <- output_list[[3]]
    
    # Reconstruction loss (only for observed values)
    recon_loss <- nnf_mse_loss(reconstruction * mask, input * mask, reduction = "sum") / torch_sum(mask)
    
    # KL Divergence
    kl_div <- -0.5 * torch_mean(1 + logvar - mu^2 - torch_exp(logvar)) * 2  
    
    recon_loss + kl_div
  }
  
  # Model initialization
  input_dim <- ncol(data_tensor)
  vae <- vae_module(input_dim, latent_dim)
  optimizer <- optim_adam(vae$parameters, lr = lr)
  
  # Training Loop
  for (epoch in seq_len(epochs)) {
    optimizer$zero_grad()
    output_list <- vae(data_tensor)
    loss <- vae_loss(output_list, data_tensor, mask_tensor)
    loss$backward()
    optimizer$step()
  }
  
  # Generate `m` imputed datasets
  imputed_datasets <- list()
  
  for (i in 1:m) {
    output_list <- vae(data_tensor)  
    imputed_data <- as_array(output_list[[1]]$detach())  
    
    # Fill in missing values with VAE imputations
    final_data <- data_matrix
    final_data[missing_mask == 0] <- imputed_data[missing_mask == 0]
    
    imputed_datasets[[i]] <- as.data.frame(final_data)
  }
  
  return(imputed_datasets)
}

# ---- Generate Missing Data ----
set.seed(123)
n <- 500
p <- 49  
rho <- 0.5  
Sigma <- matrix(rho, nrow = p, ncol = p)
diag(Sigma) <- 1
mu <- rep(0, p)  
data <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)
df <- as.data.frame(data)

# Generate outcome variable
df$y <- as.matrix(data) %*% c(rep(0.2, 4), rep(0, p-4)) + rnorm(n, 0, 1)

df$y <- as.numeric(df$y)
df <- as.data.frame(df)

# Introduce missing values (50% MCAR)
amputed_data <- ampute(df, prop = 0.5, mech = "MCAR")$amp


# Introduce missing values (50% MCAR)
amputed_data <- ampute(df, prop = 0.5, mech = "MCAR")$amp  

# Train VAE and generate 5 imputed datasets
imputed_list <- train_vae_and_impute(amputed_data, m = 5, epochs = 100, latent_dim = 2, lr = 0.001)

evaluate_vae_imputation <- function(imputed_list, true_coefs) {
  models <- lapply(imputed_list, function(data) lm(y ~ V1 + V2 + V3 + V4, data = data))
  
  # Extract coefficients for bias calculation
  coef_list <- lapply(models, coef)
  
  # Compute RMSE for each imputed dataset
  rmse_values <- sapply(models, function(mod) {
    preds <- predict(mod, newdata = imputed_list[[1]])
    sqrt(mean((imputed_list[[1]]$y - preds)^2))  # RMSE formula without Metrics package
  })
  
  # Compute bias per imputation (not pooled yet)
  bias_list <- lapply(coef_list, function(coefs) coefs - true_coefs)
  
  # Compute confidence intervals for each model
  conf_intervals <- lapply(models, function(mod) {
    ci <- confint(mod)
    
    # Ensure it's always a matrix
    if (is.vector(ci)) {
      ci <- matrix(ci, nrow = length(ci) / 2, byrow = TRUE)
    }
    
    return(ci)
  })
  
  # Convert confidence intervals to a list, ensuring correct dimensions
  conf_intervals_matrix <- sapply(conf_intervals, function(ci) {
    if (nrow(ci) >= 2) {
      return(ci[-1, ])  # Remove intercept row
    } else {
      return(matrix(NA, nrow = 1, ncol = 2))  # Fallback for single-variable cases
    }
  }, simplify = "array")  # Ensures consistent array format
  
  # Ensure conf_intervals_matrix is structured correctly
  if (length(dim(conf_intervals_matrix)) == 3) {
    ci_lower <- apply(conf_intervals_matrix, 1, function(x) mean(x[, 1] < 0.2, na.rm = TRUE))
    ci_upper <- apply(conf_intervals_matrix, 1, function(x) mean(x[, 2] > 0.2, na.rm = TRUE))
  } else {
    ci_lower <- mean(conf_intervals_matrix[1, ] < 0.2, na.rm = TRUE)
    ci_upper <- mean(conf_intervals_matrix[2, ] > 0.2, na.rm = TRUE)
  }
  
  ci_coverage <- ci_lower & ci_upper
  
  return(list(
    rmse_values = rmse_values,
    bias_per_imputation = bias_list,
    confidence_interval_coverage = ci_coverage
  ))
}

# ---- Run Evaluation ----
true_coefs <- c(0, rep(0.2, 4))  # True regression coefficients
performance_metrics <- evaluate_vae_imputation(imputed_list, true_coefs)

# Print results
print(paste("Pooled RMSE:", mean(performance_metrics$rmse_values)))
print("Bias per Imputation:")
print(performance_metrics$bias_per_imputation)
print("Confidence Interval Coverage:")
print(performance_metrics$confidence_interval_coverage)

