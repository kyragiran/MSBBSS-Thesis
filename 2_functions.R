# 2_functions.R

library(mice)
library(torch)

# --- Imputation Functions ---

do_single_imputation <- function(data) {
  for (col in names(data)) {
    if (any(is.na(data[[col]]))) {
      data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
    }
  }
  return(data)
}

do_multiple_imputation <- function(data, m = 5) {
  imp <- mice(data, method = "pmm", m = m, maxit = 5, print = FALSE)
  return(complete(imp, "all"))
}

train_vae_and_impute <- function(data, m = 5, epochs = 100, latent_dim = 2, lr = 0.001) {
  data_matrix <- as.matrix(data)
  missing_mask <- ifelse(is.na(data_matrix), 0, 1)
  data_matrix[is.na(data_matrix)] <- 0
  data_tensor <- torch_tensor(data_matrix, dtype = torch_float())
  mask_tensor <- torch_tensor(missing_mask, dtype = torch_float())

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

  vae_loss <- function(output_list, input, mask) {
    reconstruction <- output_list[[1]]
    mu <- output_list[[2]]
    logvar <- output_list[[3]]
    recon_loss <- nnf_mse_loss(reconstruction * mask, input * mask, reduction = "sum") / torch_sum(mask)
    kl_div <- -0.5 * torch_mean(1 + logvar - mu^2 - torch_exp(logvar)) * 2
    recon_loss + kl_div
  }

  input_dim <- ncol(data_tensor)
  vae <- vae_module(input_dim, latent_dim)
  optimizer <- optim_adam(vae$parameters, lr = lr)

  for (epoch in seq_len(epochs)) {
    optimizer$zero_grad()
    output_list <- vae(data_tensor)
    loss <- vae_loss(output_list, data_tensor, mask_tensor)
    loss$backward()
    optimizer$step()
  }

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

# --- Evaluation Functions ---

evaluate_single_imputation <- function(imputed_data, true_coefs) {
  model <- lm(y ~ V1 + V2 + V3 + V4, data = imputed_data)
  ci <- confint(model)
  est <- coef(model)
  se <- summary(model)$coefficients[, "Std. Error"]
  ci_coverage <- (ci[, 1] < true_coefs) & (ci[, 2] > true_coefs)
  rmse <- sqrt(mean((predict(model, newdata = imputed_data) - imputed_data$y)^2))
  bias <- est - true_coefs
  return(data.frame(estimate = est, std.error = se, `2.5 %` = ci[,1], `97.5 %` = ci[,2], bias = bias, rmse = rmse, coverage = ci_coverage))
}

evaluate_multiple_imputation <- function(imputed_list, true_coefs) {
  models <- lapply(imputed_list, function(data) lm(y ~ V1 + V2 + V3 + V4, data = data))
  pooled <- pool(models)
  pooled_summary <- summary(pooled, conf.int = TRUE)
  est <- pooled_summary$estimate
  se <- pooled_summary$std.error
  bias <- est - true_coefs
  coverage <- (pooled_summary$`2.5 %` < true_coefs) & (pooled_summary$`97.5 %` > true_coefs)
  rmse_list <- lapply(imputed_list, function(data) {
    mod <- lm(y ~ V1 + V2 + V3 + V4, data = data)
    sqrt(mean((predict(mod, newdata = data) - data$y)^2))
  })
  return(data.frame(estimate = est, std.error = se, `2.5 %` = pooled_summary$`2.5 %`, `97.5 %` = pooled_summary$`97.5 %`, bias = bias, rmse = mean(unlist(rmse_list)), coverage = coverage, row.names = pooled_summary$term))
}

evaluate_vae_imputation <- function(imputed_list, true_coefs) {
  models <- lapply(imputed_list, function(data) lm(y ~ V1 + V2 + V3 + V4, data = data))
  coef_list <- lapply(models, coef)
  ci_list <- lapply(models, confint)
  se_list <- lapply(models, function(mod) summary(mod)$coefficients[, "Std. Error"])
  ci_check <- lapply(ci_list, function(x) (x[,1] < true_coefs) & (x[,2] > true_coefs))
  rmse_values <- sapply(models, function(mod) {
    preds <- predict(mod, newdata = imputed_list[[1]])
    sqrt(mean((imputed_list[[1]]$y - preds)^2))
  })
  bias_list <- lapply(coef_list, function(coefs) coefs - true_coefs)
  return(data.frame(
    estimate = rowMeans(do.call(cbind, coef_list)),
    std.error = rowMeans(do.call(cbind, se_list)),
    `2.5 %` = rowMeans(do.call(cbind, lapply(ci_list, function(x) x[, 1]))),
    `97.5 %` = rowMeans(do.call(cbind, lapply(ci_list, function(x) x[, 2]))),
    bias = rowMeans(do.call(cbind, bias_list)),
    rmse = mean(rmse_values),
    coverage = rowMeans(do.call(cbind, ci_check)),
    row.names = names(coef_list[[1]])
  ))
}

# Optional functions retained for testing

generate_data <- function() {
  readRDS("original_data.RDS")
}

create_missing_data <- function(data, prop, mech) {
  ampute(data, prop = prop, mech = mech)$amp
}
