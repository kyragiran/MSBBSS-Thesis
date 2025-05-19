

# Load necessary packages
library(tidyverse)
library(janitor)

# List all result CSV files
files <- list.files(pattern = "^(MEAN|PMM|NORM|VAE)_.*\\.csv$")

# Define function to read and clean each file
read_and_label <- function(file) {
  df <- read_csv(file, show_col_types = FALSE) %>%
    clean_names() %>%
    rename(term = 1) %>%
    mutate(
      Method = str_extract(file, "MEAN|PMM|NORM|VAE"),
      Mechanism = if_else(str_detect(file, "MCAR"), "MCAR", "MAR"),
      Missingness = as.numeric(str_extract(file, "\\d+"))
    )
  
  # Fix coverage column name
  if ("coverage" %in% names(df)) {
    df$coverage <- df$coverage
  } else if ("cov" %in% names(df)) {
    df$coverage <- df$cov
  } else {
    df$coverage <- NA
  }
  
  # Calculate CI width from quantile columns if available
  if (!"ci_width" %in% names(df)) {
    if (all(c("x2_5", "x97_5") %in% names(df))) {
      df <- df %>% mutate(ci_width = x97_5 - x2_5)
    } else if (all(c("x2_5_percent", "x97_5_percent") %in% names(df))) {
      df <- df %>% mutate(ci_width = x97_5_percent - x2_5_percent)
    } else if (all(c("x2_5_", "x97_5_") %in% names(df))) {
      df <- df %>% mutate(ci_width = x97_5_ - x2_5_)
    } else {
      df$ci_width <- NA
    }
  }
  
  # Final format
  df %>%
    select(term, Method, Mechanism, Missingness, estimate, std_error, bias, coverage, ci_width, rmse)
}

# Read all files
all_results <- map_dfr(files, read_and_label)

# Inspect
glimpse(all_results)



### BIAS PLOT
ggplot(all_results, aes(x = Missingness, y = bias, color = Mechanism)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ Method) +
  labs(title = "Bias by Missingness Level and Imputation Method",
       y = "Bias") +
  theme_minimal()
 
ggplot(all_results, aes(x = Missingness, y = bias, color = Mechanism)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ Method) +
  labs(title = "Bias by Missingness Level and Imputation Method",
       y = "Bias") +
  theme_minimal()

ggplot(all_results, aes(x = Missingness, y = bias, group = term)) +
  geom_line(alpha = 0.3) +  # lines per term, semi-transparent
  geom_point(alpha = 0.3) +
  stat_summary(fun = mean, geom = "line", color = "red", linewidth = 1) +  # mean bias
  facet_grid(Mechanism ~ Method) +
  labs(title = "Bias across Terms, by Method and Mechanism",
       y = "Bias", x = "Missingness (%)") +
  theme_minimal()



library(ggplot2)

ggplot(all_results, aes(x = Missingness, y = bias, color = Method)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(. ~ Mechanism) +
  labs(title = "Average Bias by Method and Missingness",
       x = "Missingness (%)", y = "Bias") +
  theme_minimal() +
  theme(legend.position = "top")


library(ggplot2)

ggplot(all_results, aes(x = Missingness, y = bias, color = Method)) +
  geom_point(alpha = 0.5, position = position_jitter(width = 1, height = 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_grid(. ~ Mechanism) +
  labs(
    title = "Bias per Method and Missingness Level",
    x = "Missingness (%)",
    y = "Bias"
  ) +
  theme_minimal() +
  theme(legend.position = "top")


ggplot(all_results, aes(x = factor(Missingness), y = bias, fill = Method)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ Mechanism) +
  labs(
    title = "Bias Distributions by Method and Missingness Level",
    x = "Missingness (%)",
    y = "Bias"
  ) +
  theme_minimal() +
  theme(legend.position = "top")




library(ggplot2)

ggplot(all_results %>% filter(term %in% c("V1", "V2", "V3", "V4")),
       aes(x = Missingness, y = bias, color = Method)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(Mechanism ~ term, scales = "free_y") +
  labs(
    title = "Bias Across Methods and Missingness Levels",
    x = "Missingness (%)",
    y = "Bias"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text = element_text(size = 10, face = "bold")
  )


ggplot(all_results, aes(x = Missingness, y = bias, color = Mechanism, group = Mechanism)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  facet_grid(term ~ Method) +
  labs(title = "Bias by Imputation Method and Missingness", y = "Bias") +
  theme_minimal()



library(tidyverse)
library(janitor)

# Step 1: Load and clean all CSVs
files <- list.files(pattern = "^(MEAN|PMM|NORM|VAE)_.*\\.csv$")

read_and_label <- function(file) { 
  df <- read_csv(file, show_col_types = FALSE) %>%
    clean_names() %>%
    rename(term = 1) %>%
    mutate(
      Method = str_extract(file, "MEAN|PMM|NORM|VAE"),
      Mechanism = if_else(str_detect(file, "MCAR"), "MCAR", "MAR"),
      Missingness = as.numeric(str_extract(file, "\\d+"))
    )
  
  # Add coverage column safely
  df$coverage <- if ("coverage" %in% names(df)) {
    df$coverage
  } else if ("cov" %in% names(df)) {
    df$cov
  } else {
    NA
  }
  
  # Handle ci_width
  if ("ci_width" %in% names(df)) {
    df$ci_width <- df$ci_width
  } else if ("ci.width" %in% names(df)) {
    df$ci_width <- df$ci.width
  } else {
    lower_ci_name <- names(df)[str_detect(names(df), "2.*5")]
    upper_ci_name <- names(df)[str_detect(names(df), "97.*5")]
    
    if (length(lower_ci_name) == 1 && length(upper_ci_name) == 1) {
      df <- df %>%
        mutate(ci_width = .data[[upper_ci_name]] - .data[[lower_ci_name]])
    } else {
      df$ci_width <- NA
    }
  }
  
  df %>%
    select(term, Method, Mechanism, Missingness, estimate, std_error, bias, coverage, ci_width, rmse)
}


all_results <- map_dfr(files, read_and_label)

glimpse(all_results)


# Step 2: Save for later use
write_csv(all_results, "full_results_combined_2.csv")

# Step 3: Plotting
metrics <- c("bias", "coverage", "ci_width")

for (metric in metrics) {
  ggplot(all_results, aes(x = Missingness, y = .data[[metric]], color = Method)) +
    geom_point(size = 2) +
    geom_line(aes(group = Method)) +
    facet_wrap(~ Mechanism) +
    theme_minimal(base_size = 14) +
    labs(title = paste("Performance by Missingness:", metric),
         y = metric, x = "Missingness (%)") +
    ggsave(paste0("plot_", metric, ".png"), width = 10, height = 6)
}



library(tidyverse)

# Define a plotting function
plot_metric <- function(data, metric_label, y_label) {
  ggplot(data, aes(x = Missingness, y = .data[[metric_label]], color = Method, group = Method)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    facet_wrap(~Mechanism, scales = "free_y") +
    theme_minimal(base_size = 13) +
    labs(
      title = paste0(str_to_title(metric_label), " by Method and Missingness"),
      x = "Missingness (%)",
      y = y_label,
      color = "Imputation Method"
    ) +
    scale_color_brewer(palette = "Set1") +
    theme(legend.position = "bottom") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray")
}

# Example plots — make sure `all_results` is your combined dataframe
bias_plot <- plot_metric(all_results, "bias", "Bias")
coverage_plot <- plot_metric(all_results, "coverage", "Coverage")
ciwidth_plot <- plot_metric(all_results, "ci_width", "95% CI Width")

# Show or save them
bias_plot
coverage_plot
ciwidth_plot


library(tidyverse)

# Load your combined data
library(tidyverse)

# Load the results
df <- read_csv("full_results_combined_2.csv", show_col_types = FALSE)

# Plot: Bias by Method (color), faceted by Mechanism, rows = term
ggplot(df, aes(x = Missingness, y = bias, color = Method, group = Method)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Mechanism + term, scales = "fixed", ncol = 5) +
  labs(
    title = "Bias by Method and Missingness (Faceted by Mechanism and Term)",
    x = "Missingness (%)",
    y = "Bias",
    color = "Imputation Method"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 9),
    legend.position = "bottom"
  )

# Improved Bias Plot with free y-axis scale
ggplot(df, aes(x = Missingness, y = bias, color = Method, group = Method)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Mechanism + term, scales = "free_y", ncol = 5) +
  labs(
    title = "Bias by Method and Missingness (Faceted by Mechanism and Term)",
    x = "Missingness (%)",
    y = "Bias",
    color = "Imputation Method"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 9),
    legend.position = "bottom"
  )


ggplot(df, aes(x = Missingness, y = bias, color = Method, group = Method)) +
  geom_line(alpha = 0.7, linewidth = 1.1) +
  geom_point(alpha = 0.7, size = 2) +
  facet_wrap(~ Mechanism + term, scales = "fixed", ncol = 5) +
  labs(
    title = "Bias by Method and Missingness (Faceted by Mechanism and Term)",
    x = "Missingness (%)",
    y = "Bias",
    color = "Imputation Method"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 9),
    legend.position = "bottom"
  )

ggplot(df, aes(x = Missingness, y = bias, color = Method, group = Method)) +
  geom_line(alpha = 0.7, linewidth = 1.1) +
  geom_point(alpha = 0.7, size = 2) +
  facet_wrap(~ Mechanism + term, scales = "fixed", ncol = 5) +
  scale_color_manual(
    values = c(
      "MEAN" = "#0072B2",  # reddish-orange
      "PMM" = "#D55E00",   # blue
      "NORM" = "#009E73",  # green
      "VAE" = "#CC79A7"    # magenta
    )
  ) +
  labs(
    title = "Bias by Method and Missingness (Faceted by Mechanism and Term)",
    x = "Missingness (%)",
    y = "Bias",
    color = "Imputation Method"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 9),
    legend.position = "bottom"
  )







ggplot(df, aes(x = Missingness, y = bias, color = Method, group = Method, shape = Method)) +
  geom_line() +
  geom_point(size = 2) +
  scale_shape_manual(values = c("MEAN" = 16, "PMM" = 17, "NORM" = 18, "VAE" = 15)) +
  facet_wrap(~ Mechanism + term, scales = "fixed", ncol = 5) +
  labs(
    title = "Bias by Method and Missingness (Faceted by Mechanism and Term)",
    x = "Missingness (%)",
    y = "Bias",
    color = "Imputation Method",
    shape = "Imputation Method"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 9),
    legend.position = "bottom"
  )



#BIAS

ggplot(df, aes(x = Missingness, y = bias, color = Method, group = Method, shape = Method, linetype = Method)) +
  geom_line(alpha = 0.6, linewidth = 1.5) +
  geom_point(size = 4) +
  scale_shape_manual(values = c("MEAN" = 16, "PMM" = 17, "NORM" = 18, "VAE" = 15)) +
  scale_linetype_manual(values = c("MEAN" = "solid", "PMM" = "dashed", "NORM" = "dotdash", "VAE" = "twodash")) +
  facet_wrap(~ Mechanism + term, scales = "fixed", ncol = 5) +
  labs(
    title = "Bias by Method and Missingness (Faceted by Mechanism and Term)",
    x = "Missingness (%)",
    y = "Bias",
    color = "Imputation Method",
    shape = "Imputation Method",
    linetype = "Imputation Method"
  ) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )



ggplot(df, aes(x = Missingness, y = bias, color = Method, group = Method, shape = Method, linetype = Method)) +
  geom_line(alpha = 0.6, linewidth = 1.5) +
  geom_point(size = 4) +
  scale_shape_manual(values = c("MEAN" = 16, "PMM" = 17, "NORM" = 18, "VAE" = 15)) +
  scale_linetype_manual(values = c("MEAN" = "solid", "PMM" = "dashed", "NORM" = "dotdash", "VAE" = "twodash")) +
  facet_wrap(~ Mechanism + term, scales = "fixed", ncol = 5) +
  labs(
    title = "Bias by Method and Missingness (Faceted by Mechanism and Term)",
    x = "Missingness (%)",
    y = "Bias",
    color = "Imputation Method",
    shape = "Imputation Method",
    linetype = "Imputation Method"
  ) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13)
  )

# Optional: create folder if it doesn't exist

# Create the bias plot and assign it to a variable
bias_plot <- ggplot(df, aes(x = Missingness, y = bias, color = Method, group = Method, shape = Method, linetype = Method)) +
  geom_line(alpha = 0.6, linewidth = 1.5) +
  geom_point(size = 4) +
  scale_shape_manual(values = c("MEAN" = 16, "PMM" = 17, "NORM" = 18, "VAE" = 15)) +
  scale_linetype_manual(values = c("MEAN" = "solid", "PMM" = "dashed", "NORM" = "dotdash", "VAE" = "twodash")) +
  facet_wrap(~ Mechanism + term, scales = "fixed", ncol = 5) +
  labs(
    title = "Bias by Method and Missingness (Faceted by Mechanism and Term)",
    x = "Missingness (%)",
    y = "Bias",
    color = "Imputation Method",
    shape = "Imputation Method",
    linetype = "Imputation Method"
  ) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13)
  )

# Save the plot
ggsave("bias_plot.png", plot = bias_plot, width = 10, height = 6.5, dpi = 300)

citation()
library(ggplot2)

#ez a jó
# Create the plot
bias_plot <- ggplot(df, aes(x = Missingness, y = bias, color = Method, group = Method, shape = Method, linetype = Method)) +
  geom_line(alpha = 0.6, linewidth = 1.5) +
  geom_point(size = 4) +
  scale_shape_manual(values = c("MEAN" = 16, "PMM" = 17, "NORM" = 18, "VAE" = 15)) +
  scale_linetype_manual(values = c("MEAN" = "solid", "PMM" = "dashed", "NORM" = "dotdash", "VAE" = "twodash")) +
  facet_grid(term ~ Mechanism, scales = "fixed")+
  labs(
    title = "Bias by Method and Missingness (Faceted by Mechanism and Term)",
    x = "Missingness (%)",
    y = "Bias",
    color = "Imputation Method",
    shape = "Imputation Method",
    linetype = "Imputation Method"
  ) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 13, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 11)
  )

# Save the plot to PDF with dimensions that match LaTeX \textwidth

print(bias_plot)
dev.off()

bias_plot <- ggplot(df, aes(x = Missingness, y = bias, color = Method, group = Method, shape = Method, linetype = Method)) +
  geom_line(alpha = 0.6, linewidth = 1.5) +
  geom_point(size = 4) +
  scale_shape_manual(values = c("MEAN" = 16, "PMM" = 17, "NORM" = 18, "VAE" = 15)) +
  scale_linetype_manual(values = c("MEAN" = "solid", "PMM" = "dashed", "NORM" = "dotdash", "VAE" = "twodash")) +
  facet_grid(term ~ Mechanism, scales = "fixed") +
  labs(
    title = "Bias by Method and Missingness (Faceted by Term and Mechanism)",
    x = "Missingness (%)",
    y = "Bias",
    color = "Imputation Method",
    shape = "Imputation Method",
    linetype = "Imputation Method"
  ) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 9),
    legend.position = "bottom"
  )

print(bias_plot)




library(readr)
library(ggplot2)

df <- read_csv("full_results_combined_2.csv", show_col_types = FALSE)

# Plot: Coverage by Method (color), faceted by Mechanism and Term
ggplot(df, aes(x = Missingness, y = coverage, color = Method, group = Method)) +
  geom_line(alpha = 0.6, linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Mechanism + term, scales = "fixed", ncol = 5) +
  labs(
    title = "Coverage by Method and Missingness (Faceted by Mechanism and Term)",
    x = "Missingness (%)",
    y = "Coverage",
    color = "Imputation Method"
  ) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "gray50") +  # reference line for 95% coverage
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),
    legend.position = "bottom"
  )

####

# Optional: create folder if it doesn't exist
if (!dir.exists("plots")) dir.create("plots")

# Create the plot and assign it to a variable
coverage_plot <- ggplot(df, aes(x = Missingness, y = coverage, color = Method, group = Method, shape = Method, linetype = Method)) +
  geom_line(alpha = 0.6, linewidth = 1.5) +
  geom_point(size = 4) +
  scale_shape_manual(values = c("MEAN" = 16, "PMM" = 17, "NORM" = 18, "VAE" = 15)) +
  scale_linetype_manual(values = c("MEAN" = "solid", "PMM" = "dashed", "NORM" = "dotdash", "VAE" = "twodash")) +
  facet_wrap(~ Mechanism + term, scales = "fixed", ncol = 5) +
  labs(
    title = "Coverage by Method and Missingness (Faceted by Mechanism and Term)",
    x = "Missingness (%)",
    y = "Coverage",
    color = "Imputation Method",
    shape = "Imputation Method",
    linetype = "Imputation Method"
  ) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "gray50") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13)
  )

# Save the plot to the "plots" folder
ggsave("coverage_plot.png", plot = coverage_plot, width = 10, height = 6.5, dpi = 300)



# javított

ggplot(df, aes(x = Missingness, y = coverage, color = Method, group = Method, shape = Method, linetype = Method)) +
  geom_line(alpha = 0.6, linewidth = 1.5) +
  geom_point(size = 4) +
  scale_shape_manual(values = c("MEAN" = 16, "PMM" = 17, "NORM" = 18, "VAE" = 15)) +
  scale_linetype_manual(values = c("MEAN" = "solid", "PMM" = "dashed", "NORM" = "dotdash", "VAE" = "twodash")) +
  facet_wrap(~ Mechanism + term, scales = "fixed", ncol = 5) +
  labs(
    title = "Coverage by Method and Missingness (Faceted by Mechanism and Term)",
    x = "Missingness (%)",
    y = "Coverage",
    color = "Imputation Method",
    shape = "Imputation Method",
    linetype = "Imputation Method"
  ) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "gray50") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13)
  )



#CI width

library(readr)
library(ggplot2)

df <- read_csv("full_results_combined_2.csv", show_col_types = FALSE)

# Plot: CI Width by Method (color), faceted by Mechanism and Term
ggplot(df, aes(x = Missingness, y = ci_width, color = Method, group = Method)) +
  geom_line(alpha = 0.6, linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Mechanism + term, scales = "fixed", ncol = 5) +
  labs(
    title = "95% Confidence Interval Width by Method and Missingness",
    x = "Missingness (%)",
    y = "CI Width",
    color = "Imputation Method"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 9),
    legend.position = "bottom"
  )

library(readr)
library(ggplot2)

df <- read_csv("full_results_combined_2.csv", show_col_types = FALSE)

# Assign the plot to an object
ci_width_plot <- ggplot(df, aes(x = Missingness, y = ci_width, color = Method, group = Method, shape = Method, linetype = Method)) +
  geom_line(alpha = 0.6, linewidth = 1.5) +
  geom_point(size = 4) +
  scale_shape_manual(values = c("MEAN" = 16, "PMM" = 17, "NORM" = 18, "VAE" = 15)) +
  scale_linetype_manual(values = c("MEAN" = "solid", "PMM" = "dashed", "NORM" = "dotdash", "VAE" = "twodash")) +
  facet_wrap(~ Mechanism + term, scales = "fixed", ncol = 5) +
  labs(
    title = "95% Confidence Interval Width by Method and Missingness",
    x = "Missingness (%)",
    y = "CI Width",
    color = "Imputation Method",
    shape = "Imputation Method",
    linetype = "Imputation Method"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13)
  )

# Save the plot
ggsave("ci_width_plot.png", plot = ci_width_plot, width = 10, height = 6.5, dpi = 300)



#javított
ggplot(df, aes(x = Missingness, y = ci_width, color = Method, group = Method, shape = Method, linetype = Method)) +
  geom_line(alpha = 0.6, linewidth = 1.5) +
  geom_point(size = 4) +
  scale_shape_manual(values = c("MEAN" = 16, "PMM" = 17, "NORM" = 18, "VAE" = 15)) +
  scale_linetype_manual(values = c("MEAN" = "solid", "PMM" = "dashed", "NORM" = "dotdash", "VAE" = "twodash")) +
  facet_wrap(~ Mechanism + term, scales = "fixed", ncol = 5) +
  labs(
    title = "95% Confidence Interval Width by Method and Missingness",
    x = "Missingness (%)",
    y = "CI Width",
    color = "Imputation Method",
    shape = "Imputation Method",
    linetype = "Imputation Method"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13)
  )






#RMSEA

# Plot: RMSEA by Method, faceted by Mechanism only
ggplot(df, aes(x = Missingness, y = rmse, color = Method, shape = Method, group = Method)) +
  geom_line(alpha = 0.6, linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Mechanism) +
  labs(
    title = "RMSE by Method and Missingness (Faceted by Mechanism)",
    x = "Missingness (%)",
    y = "RMSE",
    color = "Imputation Method",
    shape = "Imputation Method"
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )


ggplot(df, aes(x = Missingness, y = rmse, color = Method, shape = Method, group = Method)) +
  geom_line(alpha = 0.6, linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Mechanism) +
  labs(
    title = "RMSE by Method and Missingness (Faceted by Mechanism)",
    x = "Missingness (%)",
    y = "RMSE",
    color = "Imputation Method",
    shape = "Imputation Method"
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )


library(ggplot2)
library(readr)

# Read the updated RMSE data
df <- read_csv("combined_rmse_summary.csv")
ggplot(df, aes(x = Missingness, y = rmse, color = Method, shape = Method, linetype = Method, group = Method)) +
  geom_line(alpha = 0.6, linewidth = 1.5) +
  geom_point(size = 4) +
  scale_shape_manual(values = c("MEAN" = 16, "PMM" = 17, "NORM" = 18, "VAE" = 15)) +
  scale_linetype_manual(values = c("MEAN" = "solid", "PMM" = "dashed", "NORM" = "dotdash", "VAE" = "twodash")) +
  facet_wrap(~ Mechanism, scales = "fixed") +
  labs(
    title = "RMSE by Method and Missingness",
    x = "Missingness (%)",
    y = "RMSE",
    color = "Imputation Method",
    shape = "Imputation Method",
    linetype = "Imputation Method"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13)
  )

# Save a smaller version of the plot
ggsave("rmse_plot_small.png", width = 4.5, height = 3.5, dpi = 300)



# RMSE Plot with compact lines and enhanced styling
ggplot(df, aes(x = Missingness, y = rmse, color = Method, shape = Method, group = Method)) +
  geom_line(alpha = 0.6, linewidth = 1.2, position = position_dodge(width = 2)) +
  geom_point(size = 4, position = position_dodge(width = 2)) +
  facet_wrap(~ Mechanism) +
  labs(
    title = "RMSE by Imputation Method and Missingness Level",
    x = "Missingness (%)",
    y = "RMSE",
    color = "Imputation Method",
    shape = "Imputation Method"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13)
  )




# Load your .RData file
load("VAE_results (1).RData")  # adjust if needed

# Identify all VAE result objects in your environment
vae_objects <- ls(pattern = "^VAE_")

# Extract RMSE values
rmse_summary <- lapply(vae_objects, function(obj_name) {
  data <- get(obj_name)
  rmsey <- round(data$rmsey, 3)
  
  # Extract mechanism and missingness from object name
  mechanism <- if (grepl("MCAR", obj_name)) "MCAR" else "MAR"
  missingness <- as.numeric(gsub("\\D", "", obj_name))
  
  data.frame(
    Method = "VAE",
    Mechanism = mechanism,
    Missingness = missingness,
    RMSE = rmsey
  )
})

# Combine into a single data frame
rmse_df <- do.call(rbind, rmse_summary)

glimpse(rmse_df)

# Save to CSV
write.csv(rmse_df, "VAE_RMSE_extracted_2.csv", row.names = FALSE)

# Preview
print(rmse_df)

