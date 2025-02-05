install.packages("keras")
install.packages("tensorflow")
install.packages("mice")
install.packages("dplyr")

library(keras)
library(tensorflow)
library(mice)
library(dplyr)

install.packages("reticulate")
library(reticulate)
reticulate::install_miniconda()


# Install TensorFlow backend if needed
tensorflow::install_tensorflow()

# Load 'boys' dataset from mice
data("boys", package = "mice")

# Check missing data
summary(boys)
md.pattern(boys)

# Convert categorical variables to numeric
boys_numeric <- boys %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.numeric))

# Handle missing values: Fill with column mean temporarily
boys_numeric[is.na(boys_numeric)] <- apply(boys_numeric, 2, function(x) mean(x, na.rm = TRUE))

# Normalize data
boys_scaled <- boys_numeric %>%
  mutate(across(where(is.numeric), ~ scale(.) %>% as.vector()))

# Convert to matrix
boys_matrix <- as.matrix(boys_scaled)

# Model parameters
input_dim <- ncol(boys_matrix)
latent_dim <- 2  # Adjust for complexity

# Encoder
encoder_input <- layer_input(shape = input_dim)
hidden <- layer_dense(encoder_input, units = 64, activation = "relu")
z_mean <- layer_dense(hidden, units = latent_dim)
z_log_var <- layer_dense(hidden, units = latent_dim)

# Reparameterization trick
sampling <- function(args) {
  z_mean <- args[[1]]
  z_log_var <- args[[2]]
  epsilon <- k_random_normal(shape = c(k_shape(z_mean)[1], latent_dim), mean = 0, stddev = 1)
  z_mean + k_exp(z_log_var / 2) * epsilon
}

z <- layer_lambda(list(z_mean, z_log_var), sampling)

# Decoder
decoder_hidden <- layer_dense(units = 64, activation = "relu")
decoder_output <- layer_dense(units = input_dim, activation = "linear")

# Instantiate the encoder and decoder
encoder <- keras_model(encoder_input, z)
decoder <- keras_model(decoder_hidden, decoder_output)

# VAE model
output <- decoder(decoder_hidden(z))
vae <- keras_model(encoder_input, output)
