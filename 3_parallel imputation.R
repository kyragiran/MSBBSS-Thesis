library(parallel)
library(foreach)
library(doParallel)
library(mice)
library(MASS)
library(Matrix)
library(torch)
library(dplyr)
library(ggplot2)
library(Metrics)

source("imputation_functions.R")
setwd("C://Users//giran//OneDrive//Dokumentumok//UU MSBBSS//THESIS//Codes")

# Load missing datasets
missing_datasets <- readRDS("missing_datasets.RDS")

num_simulations <- 50
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

run_simulation <- function(sim_id, dataset_name, dataset) {
  set.seed(123 + sim_id)
  
  imp_single <- do_single_imputation(dataset)
  imp_multiple <- do_multiple_imputation(dataset)
  imp_vae <- train_vae_and_impute(dataset, m = 5, epochs = 100, latent_dim = 2, lr = 0.001)
  
  true_coefs <- c(0, rep(0.2, 4))
  
  performance_single <- evaluate_vae_imputation(list(imp_single), true_coefs)
  performance_multiple <- evaluate_vae_imputation(imp_multiple, true_coefs)
  performance_vae <- evaluate_vae_imputation(imp_vae, true_coefs)
  
  return(data.frame(
    sim_id = sim_id,
    dataset_name = dataset_name,
    RMSE_Single = mean(performance_single$rmse_values),
    RMSE_Multiple = mean(performance_multiple$rmse_values),
    RMSE_VAE = mean(performance_vae$rmse_values)
  ))
}

results <- foreach(sim_id = 1:num_simulations, .combine = rbind, .packages = c("mice", "MASS", "Matrix", "torch", "dplyr", "ggplot2", "Metrics")) %dopar% {
  do.call(rbind, lapply(names(missing_datasets), function(name) {
    run_simulation(sim_id, name, missing_datasets[[name]])
  }))
}

stopCluster(cl)
write.csv(results, "simulation_results.csv", row.names = FALSE)
print(summary(results))
