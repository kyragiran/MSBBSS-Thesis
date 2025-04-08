# parallel_simulation.R (updated to use improved evaluation functions)

library(mice)
library(torch)
library(dplyr)
library(foreach)
library(doParallel)
library(MASS)

source("2_functions.R")

# Set up parallel backend
num_simulations <- 50
num_cores <- parallel::detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Load missing data
missing_datasets <- readRDS("missing_datasets.RDS")
true_coefs <- c("(Intercept)" = 0, V1 = 0.2, V2 = 0.2, V3 = 0.2, V4 = 0.2)

# Function to run a single simulation for a dataset
run_simulation <- function(sim_id, dataset_name, dataset) {
  set.seed(100 + sim_id)

  imp_single <- do_single_imputation(dataset)
  imp_multiple <- do_multiple_imputation(dataset)
  imp_vae <- train_vae_and_impute(dataset, m = 5)

  eval_single <- evaluate_single_imputation(imp_single, true_coefs)
  eval_multiple <- evaluate_multiple_imputation(imp_multiple, true_coefs)
  eval_vae <- evaluate_vae_imputation(imp_vae, true_coefs)

  eval_single$method <- "single"
  eval_multiple$method <- "multiple"
  eval_vae$method <- "vae"

  results <- bind_rows(eval_single, eval_multiple, eval_vae, .id = NULL)
  results$sim_id <- sim_id
  results$dataset <- dataset_name
  results$term <- rownames(results)
  rownames(results) <- NULL
  return(results)
}

# Run all simulations
all_results <- foreach(sim_id = 1:num_simulations, .combine = rbind, .packages = c("mice", "torch", "dplyr")) %dopar% {
  do.call(rbind, lapply(names(missing_datasets), function(name) {
    run_simulation(sim_id, name, missing_datasets[[name]])
  }))
}

stopCluster(cl)

# Save to CSV
write.csv(all_results, "simulation_results_detailed.csv", row.names = FALSE)

# View summary
print(dplyr::group_by(all_results, method, term) %>% summarise(mean_coverage = mean(coverage), .groups = "drop"))
