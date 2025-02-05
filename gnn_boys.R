install.packages("gnn")
install.packages("mice")
install.packages("dplyr")

install.packages("remotes")
remotes::install_github("cran/gnn")


library(gnn)
library(mice)
library(dplyr)

# Load 'boys' dataset from mice package
data("boys", package = "mice")

# Check missing data
summary(boys)
md.pattern(boys)  # Visualize missing data structure

# Convert categorical variables to numeric
boys_numeric <- boys %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.numeric))

# Handle missing values by replacing NAs with column mean (for now)
boys_numeric[is.na(boys_numeric)] <- apply(boys_numeric, 2, function(x) mean(x, na.rm = TRUE))

# Convert to matrix
boys_matrix <- as.matrix(boys_numeric)


# Train a Variational Autoencoder (VAE)
vae <- VAE_model(
  x = boys_matrix,      # Input data
  epochs = 100,         # Number of training iterations
  latent_size = 2,      # Size of the latent space
  hidden_units = 64,    # Number of neurons in hidden layers
  verbose = TRUE        # Show training progress
)

ls("package:gnn")

