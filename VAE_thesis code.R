library(torch)
library(dplyr)
library(ggplot2)

set.seed(123)  
n <- 500
p <- 49  

# Generate synthetic correlated data
rho <- 0.5  
Sigma <- matrix(rho, nrow = p, ncol = p)
diag(Sigma) <- 1

mu <- rep(0, p)  
data <- MASS::mvrnorm(n = n, mu = mu, Sigma = Sigma)

df <- as.data.frame(data)
df$y <- c(as.matrix(data) %*% c(rep(0.2, 4), rep(0, p-4)) + rnorm(n, 0, 1))

# Introduce missingness (50% MCAR)
amputed_data <- mice::ampute(df, prop = 0.5, mech = "MCAR")$amp  

# Convert to matrix
data_matrix <- as.matrix(amputed_data)

# Create a missing mask (1 for observed, 0 for missing)
missing_mask <- ifelse(is.na(data_matrix), 0, 1)

# Replace NA with zeros (placeholder for missing values)
data_matrix[is.na(data_matrix)] <- 0

# Convert to torch tensors
data_tensor <- torch_tensor(data_matrix, dtype = torch_float())
mask_tensor <- torch_tensor(missing_mask, dtype = torch_float())

# 2) Define Encoder and Decoder Modules ---------------------------------

encoder_module <- nn_module(
  "encoder_module",
  
  initialize = function(in_features, latent_dim) {
    self$model <- nn_sequential(
      nn_linear(in_features, 64),
      nn_relu(),
      nn_linear(64, latent_dim * 2)  # => [mu, logvar]
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

# 3) VAE module composing Encoder & Decoder -----------------------------

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

# 4) Modified Loss Function (MSE + KL) with Missing Data Handling -------
vae_loss <- function(output_list, input, mask) {
  reconstruction <- output_list[[1]]
  mu <- output_list[[2]]
  logvar <- output_list[[3]]
  
  # Reconstruction loss (only for observed values)
  recon_loss <- nnf_mse_loss(reconstruction * mask, input * mask, reduction = "sum") / torch_sum(mask)
  
  # KL Divergence
  kl_div <- -0.5 * torch_mean(1 + logvar - mu^2 - torch_exp(logvar))
  
  recon_loss + kl_div
}

# 5) Instantiate the Model and Optimizer --------------------------------
input_dim <- ncol(data_tensor)
latent_dim <- 2

vae <- vae_module(input_dim, latent_dim)
optimizer <- optim_adam(vae$parameters, lr = 0.001)

# 6) Training Loop ------------------------------------------------------
epochs <- 100

for (epoch in seq_len(epochs)) {
  optimizer$zero_grad()
  
  output_list <- vae(data_tensor)       
  loss <- vae_loss(output_list, data_tensor, mask_tensor)  # Masked loss
  
  loss$backward()                         
  optimizer$step()                        
  
  if (epoch %% 10 == 0) {
    cat("Epoch:", epoch, "Loss:", loss$item(), "\n")
  }
}

# 7) Imputation ---------------------------------------------------------
output_list <- vae(data_tensor)  # Run trained model
imputed_data <- as_array(output_list[[1]]$detach())  # Convert to R matrix


# Fill in missing values with VAE imputations
final_data <- data_matrix
final_data[missing_mask == 0] <- imputed_data[missing_mask == 0]

# Convert to dataframe
final_df <- as.data.frame(final_data)

final_df
