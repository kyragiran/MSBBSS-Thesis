# 2_functions.R

library(mice)
library(torch)
library(MASS)

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
  return(lapply(1:m, function(i) complete(imp, i)))
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
  mod_summary <- summary(model)
  est <- coef(model)
  se <- mod_summary$coefficients[, "Std. Error"]
  ci <- confint(model)
  coverage <- (ci[, 1] < true_coefs[rownames(ci)]) & (ci[, 2] > true_coefs[rownames(ci)])
  rmse <- sqrt(mean((predict(model, newdata = imputed_data) - imputed_data$y)^2))
  bias <- est - true_coefs[names(est)]
  result <- data.frame(estimate = est, std.error = se, bias = bias, coverage = coverage, rmse = rmse)
  rownames(result) <- names(est)
  return(result)
}

evaluate_multiple_imputation <- function(imputed_list, true_coefs) {
  models <- lapply(imputed_list, function(data) lm(y ~ V1 + V2 + V3 + V4, data = data))
  pooled <- pool(models)
  pooled_summary <- summary(pooled, conf.int = TRUE)
  pooled_summary$bias <- pooled_summary$estimate - true_coefs[pooled_summary$term]
  pooled_summary$coverage <- (pooled_summary$`2.5 %` < true_coefs[pooled_summary$term]) & (pooled_summary$`97.5 %` > true_coefs[pooled_summary$term])
  pooled_summary$rmse <- mean(sapply(imputed_list, function(data) {
    mod <- lm(y ~ V1 + V2 + V3 + V4, data = data)
    sqrt(mean((predict(mod, newdata = data) - data$y)^2))
  }))
  result <- pooled_summary[, c("term", "estimate", "std.error", "bias", "coverage", "rmse")]
  rownames(result) <- result$term
  result$term <- NULL
  return(result)
}

evaluate_vae_imputation <- function(imputed_list, true_coefs) {
  terms <- names(true_coefs)
  results <- lapply(imputed_list, function(data) {
    model <- lm(y ~ V1 + V2 + V3 + V4, data = data)
    summary_model <- summary(model)
    list(est = coef(model), var = summary_model$coefficients[, "Std. Error"]^2)
  })
  
  pooled_results <- lapply(terms, function(term) {
    ests <- sapply(results, function(x) x$est[term])
    vars <- sapply(results, function(x) x$var[term])
    #browser()
    ncases <- imputed_list[[1]] %>% nrow() # number of complete data cases
    nparam <- length(terms) # number of model parameters
    pooled <- pool.scalar(Q = ests, U = vars, n = ncases - nparam)
    ci_low <- pooled$qbar - qt(0.975, df = pooled$df) * sqrt(pooled$t)
    ci_up <- pooled$qbar + qt(0.975, df = pooled$df) * sqrt(pooled$t)
    #browser()
    ci_width <- ci_up - ci_low
    data.frame(
      estimate = pooled$qbar,
      std.error = sqrt(pooled$t),
      bias = pooled$qbar - true_coefs[term],
      low = ci_low,
      up = ci_up,
      coverage = (ci_low < true_coefs[term]) & (ci_up > true_coefs[term]),
      ci.width = ci_width,
      within.var = pooled$ubar,
      between.var = pooled$b,
      total.var = pooled$t,
      rmse = sqrt(mean((ests - true_coefs[term])^2)),
      row.names = term
    )
  })
  return(do.call(rbind, pooled_results))
}
