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

return(data.frame(
    sim_id = sim_id,
    dataset_name = dataset_name,
    RMSE_Single = mean(performance_single$rmse_values),
    Bias_Single = mean(sapply(performance_single$bias_per_imputation, mean)),
    CI_Coverage_Single = mean(unlist(performance_single$confidence_interval_coverage)),
    RMSE_Multiple = mean(performance_multiple$rmse_values),
    Bias_Multiple = mean(sapply(performance_multiple$bias_per_imputation, mean)),
    CI_Coverage_Multiple = mean(unlist(performance_multiple$confidence_interval_coverage)),
    RMSE_VAE = mean(performance_vae$rmse_values),
    Bias_VAE = mean(sapply(performance_vae$bias_per_imputation, mean)),
    CI_Coverage_VAE = mean(unlist(performance_vae$confidence_interval_coverage))
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
