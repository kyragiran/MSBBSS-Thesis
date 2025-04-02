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
  ci_coverage <- (ci[2:5, 1] < true_coefs[2:5]) & (ci[2:5, 2] > true_coefs[2:5])
  rmse <- sqrt(mean((predict(model, newdata = imputed_data) - imputed_data$y)^2))
  bias <- est[2:5] - true_coefs[2:5]
  return(list(rmse_values = rmse, bias = bias, confidence_interval_coverage = ci_coverage))
}

evaluate_multiple_imputation <- function(imputed_list, true_coefs) {
  estimate <- lapply(imputed_list, function(data) 
    lm(y ~ V1 + V2 + V3 + V4, data = data)) %>% 
    pool(custom.t = ".data$b + .data$b / .data$m") %>% # pool coefficients
    .$pooled %>% # extract table of pooled coefficients
    mutate(true = true_coefs, # add true
           df = m-1,  # correct df
           riv = Inf, # correct riv
           std.error = sqrt(t), # standard error
           statistic = estimate / std.error, # test statistic
           p.value = 2 * (pt(abs(statistic), 
                             pmax(df, 0.001), 
                             lower.tail = FALSE)), # correct p.value
           `2.5 %` = estimate - qt(.975, df) * std.error, # lower bound CI
           `97.5 %` = estimate + qt(.975, df) * std.error, # upper bound CI
           cov = `2.5 %` < true & true < `97.5 %`, # coverage
           bias = estimate - true) %>% # bias
    select(term, m, true, estimate, std.error, statistic, p.value, 
           riv, `2.5 %`, `97.5 %`, cov, bias) %>% 
    column_to_rownames("term")
  browser()
  # pooled_summary <- summary(pooled, conf.int = TRUE)
  # pooled_est <- pooled_summary$estimate[2:5]
  # lower <- pooled_summary$`2.5 %`[2:5]
  # upper <- pooled_summary$`97.5 %`[2:5]
  # ci_coverage <- (lower < true_coefs[2:5]) & (upper > true_coefs[2:5])
  # bias <- pooled_est - true_coefs[2:5]
  rmse_list <- lapply(imputed_list, function(data) {
    mod <- lm(y ~ V1 + V2 + V3 + V4, data = data)
    sqrt(mean((predict(mod, newdata = data) - data$y)^2))
  })
  return(estimate %>% mutate(rmse = rmse_list))
}

evaluate_vae_imputation <- function(imputed_list, true_coefs) {
  models <- lapply(imputed_list, function(data) lm(y ~ V1 + V2 + V3 + V4, data = data))
  coef_list <- lapply(models, coef)
  CI_list <- lapply(models, confint)
  ci_check <- lapply(CI_list, function(x) (x[2:5, 1] < true_coefs[2:5]) & (x[2:5, 2] > true_coefs[2:5]))
  rmse_values <- sapply(models, function(mod) {
    preds <- predict(mod, newdata = imputed_list[[1]])
    sqrt(mean((imputed_list[[1]]$y - preds)^2))
  })
  bias_list <- lapply(coef_list, function(coefs) coefs[2:5] - true_coefs[2:5])
  return(list(rmse_values = mean(rmse_values), bias = rowMeans(do.call(cbind, bias_list)), confidence_interval_coverage = rowMeans(do.call(cbind, ci_check))))
}

