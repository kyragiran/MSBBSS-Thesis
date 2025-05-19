#Data extraction from results files

setwd("C:/Users/giran/OneDrive/Dokumentumok/UU MSBBSS/THESIS")
load("SINGLE_results.RData")


load("PMM_results.RData")

write.csv(PMM_MCAR10, "PMM_MCAR10.csv")
write.csv(PMM_MCAR25, "PMM_MCAR25.csv")
write.csv(PMM_MCAR50, "PMM_MCAR50.csv")
write.csv(PMM_MAR10, "PMM_MAR10.csv")
write.csv(PMM_MAR25, "PMM_MAR25.csv")
write.csv(PMM_MAR50, "PMM_MAR50.csv")

# Load and export MEAN results
load("MEAN_results.RData")
write.csv(MEAN_MCAR10, "MEAN_MCAR10.csv")
write.csv(MEAN_MCAR25, "MEAN_MCAR25.csv")
write.csv(MEAN_MCAR50, "MEAN_MCAR50.csv")
write.csv(MEAN_MAR10, "MEAN_MAR10.csv")
write.csv(MEAN_MAR25, "MEAN_MAR25.csv")
write.csv(MEAN_MAR50, "MEAN_MAR50.csv")

# Load and export PMM results
load("PMM_results.RData")
write.csv(PMM_MCAR10, "PMM_MCAR10.csv")
write.csv(PMM_MCAR25, "PMM_MCAR25.csv")
write.csv(PMM_MCAR50, "PMM_MCAR50.csv")
write.csv(PMM_MAR10, "PMM_MAR10.csv")
write.csv(PMM_MAR25, "PMM_MAR25.csv")
write.csv(PMM_MAR50, "PMM_MAR50.csv")

# Load and export NORM results
load("NORM_results.RData")
write.csv(NORM_MCAR10, "NORM_MCAR10.csv")
write.csv(NORM_MCAR25, "NORM_MCAR25.csv")
write.csv(NORM_MCAR50, "NORM_MCAR50.csv")
write.csv(NORM_MAR10, "NORM_MAR10.csv")
write.csv(NORM_MAR25, "NORM_MAR25.csv")
write.csv(NORM_MAR50, "NORM_MAR50.csv")

# Load and export VAE results
load("VAE_results.RData")
write.csv(VAE_MCAR10, "VAE_MCAR10.csv")
write.csv(VAE_MCAR25, "VAE_MCAR25.csv")
write.csv(VAE_MCAR50, "VAE_MCAR50.csv")
write.csv(VAE_MAR10, "VAE_MAR10.csv")
write.csv(VAE_MAR25, "VAE_MAR25.csv")
write.csv(VAE_MAR50, "VAE_MAR50.csv")

library(tidyverse)  # this includes purrr, dplyr, tidyr, ggplot2, etc.
library(stringr)    # for str_extract
install.packages("tidytable")
library(tidytable)


all_results <- list.files(pattern = "*.csv") %>%
  map_df(~ read.csv(.x) %>%
           mutate(
             Method = str_extract(.x, "MEAN|PMM|NORM|VAE"),
             Mechanism = str_extract(.x, "MCAR|MAR"),
             Missingness = str_extract(.x, "\\d+"),
             Missingness = paste0(Missingness, "%")
           )
  )




library(tidyr)
library(dplyr)

bias_table_MCAR <- all_results %>%
  filter(Mechanism == "MCAR") %>%
  select(Method, term, Missingness, bias) %>%
  pivot_wider(names_from = term, values_from = bias)

write.csv(bias_table_MCAR, "bias_table_MCAR.csv", row.names = FALSE)


library(dplyr)

# List of your CSV files
files <- list.files(path = "C://Users//giran//OneDrive//Dokumentumok//UU MSBBSS//THESIS//missing datasets", pattern = ".csv", full.names = TRUE)

# Function to calculate coverage for one file
calc_coverage <- function(file) {
  df <- read.csv(file)
  
  true_values <- rep(0, nrow(df))
  true_values[grepl("^V[1-4]$", df$term)] <- 0.2
  
  df$lower <- df$estimate - 1.96 * df$std.error
  df$upper <- df$estimate + 1.96 * df$std.error
  
  df$covered <- (true_values >= df$lower) & (true_values <= df$upper)
  
  coverage <- mean(df$covered)
  
  data.frame(
    file = basename(file),
    coverage = coverage
  )
}

# Apply the function to all files
coverage_results <- bind_rows(lapply(files, calc_coverage))

print(coverage_results)



