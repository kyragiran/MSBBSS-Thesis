# 3_simulation.R (final version)

library(parallel)
library(foreach)
library(doParallel)
library(torch)
library(mice)
library(dplyr)
library(tibble)
library(MASS)

source("2_functions.R")

num_simulations <- 50
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

true_coefs <- c(0, rep(0.2, 4))

# Load missing datasets once 
missing_datasets <- readRDS("missing_datasets.RDS")

#  Run simulation for one condition 
run_simulation <- function(sim_id, params) {
  set.seed(123 + sim_id)
  prop <- params$prop
  mech <- params$mech
  dataset <- missing_datasets[[paste0(mech, "_", prop * 100, "%")]]

  # Imputations
  imp_single <- do_single_imputation(dataset)
  imp_multiple <- do_multiple_imputation(dataset)
  imp_vae <- train_vae_and_impute(dataset, m = 5, epochs = 100, latent_dim = 2, lr = 0.001)

  # Evaluations
  eval_single <- evaluate_single_imputation(imp_single, true_coefs)
  eval_multiple <- evaluate_multiple_imputation(imp_multiple, true_coefs)
  eval_vae <- evaluate_vae_imputation(imp_vae, true_coefs)

  # Combine results
  list(
    sim_id = sim_id,
    missing_type = paste(mech, prop, sep = "_"),
    single = eval_single %>% 
      as_tibble(rownames = "term") %>% 
      mutate(method = "single", sim_id = sim_id, missing_type = paste(mech, prop, sep = "_")),
    multiple = eval_multiple %>% 
      as_tibble(rownames = "term") %>% 
      mutate(method = "multiple", sim_id = sim_id, missing_type = paste(mech, prop, sep = "_")),
    vae = eval_vae %>% 
      as_tibble(rownames = "term") %>% 
      mutate(method = "vae", sim_id = sim_id, missing_type = paste(mech, prop, sep = "_"))
  )
}

#  Simulation condition setup 
sim.cond <- data.frame(mech = c(rep("MAR", 3), rep("MCAR", 3)), 
                       prop = rep(c(0.1, 0.25, 0.5), 2)) %>% 
  mutate(simname = as.factor(paste(mech, prop, sep = "_")))

# Full Simulation 
results <- foreach(sim_id = 1:num_simulations, .packages = c("mice", "torch", "dplyr", "tibble")) %dopar% {
  source("2_functions.R")

  sim_results <- lapply(levels(sim.cond$simname), function(name) {
    params <- sim.cond[sim.cond$simname == name, 1:2]
    run_simulation(sim_id, params)
  })

  sim_results
}

stopCluster(cl)

# ---- Flatten results to data frame ----
data_long <- do.call(rbind, lapply(results, function(sim_list) {
  do.call(rbind, lapply(sim_list, function(res) {
    bind_rows(res$single, res$multiple, res$vae)
  }))
}))

saveRDS(results, "simulation_results_nested_list.RDS")
write.csv(data_long, "simulation_results_flat.csv", row.names = FALSE)

print("Simulation complete. Full results saved.")
