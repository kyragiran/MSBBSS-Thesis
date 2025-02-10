library(torch)
library(dplyr)
library(ggplot2)

data("mtcars")

# Normalize data
mtcars_scaled <- mtcars %>%
  mutate(across(where(is.numeric), ~ scale(.) %>% as.vector()))

# Convert to matrix and then to torch tensor
mtcars_matrix <- as.matrix(mtcars_scaled)
mtcars_tensor <- torch_tensor(mtcars_matrix, dtype = torch_float())

vae_model <- nn_module(
  "VAE",
  
  initialize = function(input_dim, latent_dim) {
    self$encoder <- nn_sequential(
      nn_linear(input_dim, 64),
      nn_relu(),
      nn_linear(64, latent_dim * 2)  # Outputs mean and log variance
    )
    
    self$decoder <- nn_sequential(
      nn_linear(latent_dim, 64),
      nn_relu(),
      nn_linear(64, input_dim)
    )
    
    self$latent_dim <- latent_dim
  },
  
  # Reparameterization
  reparameterize = function(self, mu, logvar) {
    std <- torch_exp(0.5 * logvar)
    eps <- torch_randn_like(std)
    mu + eps * std
  },
  
  # Forward pass
  forward = function(self, x) {
    h <- self$encoder(x)
    mu <- h[, 1:self$latent_dim]
    logvar <- h[, (self$latent_dim + 1):(2 * self$latent_dim)]
    z <- self$reparameterize(mu, logvar)
    output <- self$decoder(z)
    return(list(output, mu, logvar))  # Return all components
  }
)

vae_loss <- function(output_list, input) {
  output <- output_list[[1]]  
  mu <- output_list[[2]]  # Latent mean
  logvar <- output_list[[3]]  # Latent log variance
  
  recon_loss <- nnf_mse_loss(output, input)  # Reconstruction loss
  kl_div <- -0.5 * torch_mean(1 + logvar - mu^2 - torch_exp(logvar))  # KL loss
  
  return(recon_loss + kl_div)
}

# Initialize model parameters
input_dim <- ncol(mtcars_tensor)  
latent_dim <- 2  
vae <- vae_model(input_dim, latent_dim)

optimizer <- optim_adam(vae$parameters, lr = 0.001)

# Training loop
epochs <- 100
for (epoch in 1:epochs) {
  optimizer$zero_grad()
  
  output_list <- vae(mtcars_tensor)
  
  loss <- vae_loss(output_list, mtcars_tensor)
  
  loss$backward()
  optimizer$step()
  
  if (epoch %% 10 == 0) cat("Epoch:", epoch, "Loss:", loss$item(), "\n")
}

