# MSBBSS-Thesis

A repository for the MSBBSS Thesis, titled Evaluating Variational Autoencoders for Missing Data: A Focus on Inference  which presents a simulation study comparing traditional and deep learning-based imputation methods under different missing data conditions.
The thesis was supervised by Dr. Gerko Vink, Hanne Oberman and Thom Volker. 


## Table of Contents

- [Overview](#overview)
- [Features](#features)
- [Project Structure](#project-structure)
- [Installation](#installation)
- [Usage](#usage)
- [Workflow](#workflow)
- [Evaluation Outputs](#evaluation-outputs)
- [Ethics and Privacy Statement](#ethics and privacy statement)
- [Requirements](#requirements)

---

## Overview

This project evaluates five imputation strategies for handling missing data under MCAR and MAR conditions with varying levels of missingness (10%, 25%, 50%):

- **Mean Imputation** (`method = "mean"`)
- **Single Imputation** (`method = "norm.predict"`)
- **Multiple Imputation via PMM** (`method = "pmm"`)
- **Multiple Imputation via Bayesian regression** (`method = "norm"`)
- **Variational Autoencoder (VAE)** (deep learning approach)

Each method is implemented in a separate Quarto (`.qmd`) simulation script that:
- Generates synthetic data
- Introduces missing values
- Performs imputation
- Applies a linear model
- Evaluates inferential performance
- Saves `.RData` result files

---

## Features

- Simulates missing data under MCAR and MAR
- Evaluates 5 imputation methods on bias, RMSE, CI width, and coverage
- Uses multiple imputation with `mice`, single imputation, and custom VAE
- Parallel processing enabled via `furrr`
- Final metrics exported as `.csv` and visualized using `ggplot2`

---

## Project Structure

```
- Simulate_Mean.qmd           # Mean imputation (mice, m = 1)
- Simulate_Single.qmd         # Single imputation using norm.predict
- Simulate_PMM.qmd            # Multiple imputation via PMM
- Simulate_Norm.qmd           # Multiple imputation via norm
- Simulate_VAE.qmd            # Deep learning imputation via VAE
- Extracting results.R        # Loads evaluated .RData files and exports CSVs
- Data visulaizations.R       # Plots performance metrics from CSVs
- 2_functions.R               # Contains all imputation & evaluation functions
- /results                    # Contains evaluated result objects (.RData)
- README.md                   # This file
```


### Run Simulations

- `Simulate_Mean.qmd`
- `Simulate_Single.qmd`
- `Simulate_PMM.qmd`
- `Simulate_Norm.qmd`
- `Simulate_VAE.qmd`

Each simulation script:
- Generates and imputes 1000 datasets
- Evaluates results using custom functions from `2_functions.R`
- Saves a `.RData` file with aggregated metrics (e.g., `PMM_results.RData`)

There is **no need to run evaluation separately** — it's done in the `.qmd`.

### Extract Results for Visualization

After all simulations have run:

```r
source("Extracting results.R")
```

This script:
- Loads the `.RData` files
- Extracts bias, coverage, CI width, RMSE
- Combines all methods into:
  - `bias_df`
  - `coverage_df`
  - `ciwidth_df`
  - `rmse_df`
- Saves results as `.csv` in `/csv/`

### Visualize the Results

```r
source("Data visulaizations.R")
```

Generates `ggplot2` visualizations per metric, method, mechanism, and missingness level.

---

## Evaluation Outputs

| Metric         | Description                               |
|----------------|-------------------------------------------|
| Bias           | Mean difference from true regression coefficients |
| RMSE           | Root Mean Squared Error of predictions     |
| CI Coverage    | Proportion of 95% CIs that contain the true value |
| CI Width       | Average width of 95% confidence intervals  |

---

---

## Ethics and Privacy Statement

This repository contains only synthetic, simulated data generated for research purposes.
No personal, sensitive, or identifiable information is used or included at any point in the project.
Therefore, there are no privacy concerns related to this study.

An FETC protocol was registered under Case Number: 24-2067.
---

## Requirements:

R version: 4.4.2

R packages:
- mice (≥ 3.16.0)
- dplyr (≥ 1.1.4)
- tidyr (≥ 1.3.0)
- purrr (≥ 1.0.2)
- magrittr (≥ 2.0.3)
- furrr (≥ 0.3.1)
- MASS (≥ 7.3-60.0)
- tibble (≥ 3.2.1)
- ggplot2 (≥ 3.5.0)
- torch (≥ 0.11.0)
- mvtnorm (≥ 1.2-4)
- gnn (if used, version unknown)
- reticulate (optional, if using Python backends for VAE)

Other tools:
- Quarto ≥ 1.4.550 (for running `.qmd` files)
---


