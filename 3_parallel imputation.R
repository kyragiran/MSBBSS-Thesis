# 3_simulation.R


library(parallel)
library(foreach)
library(doParallel)
library(torch)
library(mice)
library(progressr)
library(dplyr)
library(tibble)
library(MASS)

source("2_functions.R")

#missing_datasets <- readRDS("missing_datasets.RDS")

num_simulations <- 50
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

true_coefs <- c(0, rep(0.2, 4))

run_simulation <- function(sim_id, params) {
  set.seed(123 + sim_id)
  prop <- params$prop
  mech <- params$mech
  dataset <- create_missing_data(generate_data(), prop, mech)
  imp_single <- do_single_imputation(dataset)
  imp_multiple <- do_multiple_imputation(dataset)
  imp_vae <- train_vae_and_impute(dataset, m = 5, epochs = 100, latent_dim = 2, lr = 0.001)
  performance_single <- evaluate_single_imputation(imp_single, true_coefs)
  performance_multiple <- evaluate_multiple_imputation(imp_multiple, true_coefs)
  performance_vae <- evaluate_vae_imputation(imp_vae, true_coefs)
  data.frame(
    sim_id = sim_id,
    dataset_name = paste(mech, prop, sep="_"),
    RMSE_Single = performance_single$rmse_values,
    Bias_Single = mean(performance_single$bias),
    CI_Coverage_Single = mean(performance_single$confidence_interval_coverage),
    RMSE_Multiple = performance_multiple$rmse_values,
    Bias_Multiple = mean(performance_multiple$bias),
    CI_Coverage_Multiple = mean(performance_multiple$confidence_interval_coverage),
    RMSE_VAE = performance_vae$rmse_values,
    Bias_VAE = mean(performance_vae$bias),
    CI_Coverage_VAE = mean(performance_vae$confidence_interval_coverage)
  )
}

handlers(global = TRUE)
handlers("txtprogressbar")

sim.cond <- data.frame(mech = c(rep("MAR", 3), rep("MCAR", 3)), 
                       prop = rep(c(0.1, 0.25, 0.5), 2)) %>% 
  mutate(simname = as.factor(paste(mech, prop, sep="_")))

results <- with_progress({
  p <- progressor(steps = num_simulations)
  foreach(sim_id = 1:num_simulations, .combine = rbind, .packages = c("mice", "torch")) %dopar% {
    source("2_functions.R")
    res <- do.call(rbind, lapply(levels(sim.cond[, 3]), function(x) {
      run_simulation(sim_id, sim.cond[sim.cond[, 3] == x, 1:2])}))
    p()
    res
  }
})

stopCluster(cl)
write.csv(results, "simulation_results.csv", row.names = FALSE)
print(summary(results))
