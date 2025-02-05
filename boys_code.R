library(torch)
library(mice)
library(dplyr)

library(torch)
torch::install_torch(uninstall = TRUE)

install.packages("torch")
torch::install_torch()

library(torch)
x <- torch_tensor(matrix(1:9, nrow=3, ncol=3), dtype = torch_float())
print(x)


# Load required libraries

data("boys", package = "mice")

torch_tensor(matrix(1:9, nrow=3, ncol=3), dtype = torch_float())

# Check missing data patterns
summary(boys)
md.pattern(boys)  

# Convert categorical variables to factors, then numeric
boys_numeric <- boys %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.numeric))

# Handle missing values by replacing NAs with the column mean (for now)
boys_numeric[is.na(boys_numeric)] <- apply(boys_numeric, 2, function(x) mean(x, na.rm = TRUE))

# Normalize the data 
boys_scaled <- boys_numeric %>%
  mutate(across(where(is.numeric), ~ scale(.) %>% as.vector()))

boys_matrix <- as.matrix(boys_scaled)
boys_matrix <- as.matrix(sapply(boys_numeric, as.numeric))


# Convert matrix to torch tensor
boys_tensor <- torch_tensor(boys_matrix, dtype = torch_float())


# VAE

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
  
  # Reparameterization trick
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
    self$decoder(z)
  }
)

# Initialize Model

# Define model parameters
input_dim <- ncol(boys_tensor)  # Number of features
latent_dim <- 2  # Adjust based on dataset complexity
vae <- vae_model(input_dim, latent_dim)

# Define loss function (Reconstruction Loss + KL Divergence)
vae_loss <- function(output, input, mu, logvar) {
  recon_loss <- nnf_mse_loss(output, input)  # Reconstruction loss
  kl_div <- -0.5 * torch_mean(1 + logvar - mu^2 - torch_exp(logvar))  # KL divergence loss
  recon_loss + kl_div
}

# Train the Model

# Set optimizer
optimizer <- optim_adam(vae$parameters, lr = 0.001)

# Training loop
epochs <- 100
for (epoch in 1:epochs) {
  optimizer$zero_grad()
  
  # Forward pass
  output <- vae(boys_tensor)
  
  # Compute loss
  loss <- vae_loss(output, boys_tensor)
  
  # Backpropagation
  loss$backward()
  optimizer$step()
  
  # Print progress every 10 epochs
  if (epoch %% 10 == 0) cat("Epoch:", epoch, "Loss:", loss$item(), "\n")
}
