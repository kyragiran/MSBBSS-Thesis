do_single_imputation <- function(data) {
  for (col in names(data)) {
    if (any(is.na(data[[col]]))) {
      data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
    }
  }
  return(data)
}

do_multiple_imputation <- function(data, m = 5) {
  imp <- mice(data, method = "pmm", m = m, maxit = 5)
  return(lapply(1:m, function(i) complete(imp, i)))
}



# VAE Training and Imputation Function
train_vae_and_impute <- function(data, m = 5, epochs = 100, latent_dim = 2, lr = 0.001) {
  data_matrix <- as.matrix(data)
  missing_mask <- ifelse(is.na(data_matrix), 0, 1)
  data_matrix[is.na(data_matrix)] <- 0
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
  
  # VAE model
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
  
  # Loss function
  vae_loss <- function(output_list, input, mask) {
    reconstruction <- output_list[[1]]
    mu <- output_list[[2]]
    logvar <- output_list[[3]]
    recon_loss <- nnf_mse_loss(reconstruction * mask, input * mask, reduction = "sum") / torch_sum(mask)
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
  
  # Generate imputed datasets
  imputed_datasets <- list()
  for (i in 1:m) {
    output_list <- vae(data_tensor)
    imputed_data <- as_array(output_list[[1]]$detach())  
    final_data <- data_matrix
    final_data[missing_mask == 0] <- imputed_data[missing_mask == 0]
    imputed_datasets[[i]] <- as.data.frame(final_data)
  }
  return(imputed_datasets)
}

# Evaluation Function
evaluate_vae_imputation <- function(imputed_list, true_coefs) {
  models <- lapply(imputed_list, function(data) lm(y ~ V1 + V2 + V3 + V4, data = data))
  coef_list <- lapply(models, coef)
  rmse_values <- sapply(models, function(mod) {
    preds <- predict(mod, newdata = imputed_list[[1]])
    sqrt(mean((imputed_list[[1]]$y - preds)^2))
  })
  bias_list <- lapply(coef_list, function(coefs) coefs - true_coefs)
  return(list(rmse_values = rmse_values, bias_per_imputation = bias_list))
}

