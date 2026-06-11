# Chen, E. K., Som, N. A., Deibner-Hanson, J. D., Anderson, D. G., & Henderson, M. J. (2023). A life cycle model for evaluating 
# estuary residency and recovery potential in Chinook salmon. Fisheries Research, 257, 106511.

# Code generated with Gemini 3.1 Pro on 6/11/26 using a custom GEM 'SRF Data Extraction' by AH Fullerton

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Define known parameters from the text
beta_1 <- 0.396
beta_3 <- 2.270
gamma_1 <- -0.008
gamma_3 <- 0.246
gamma_4 <- -0.066

# 2. Assume intercepts/scaling to match the visual output of Figure 2 
# (Covariates were likely z-scored in the original paper)
beta_0_adj <- -5.5     
gamma_0_adj <- -160    

# Define standard total outmigrant abundances (R) based on text averages
R_levels <- list(
  "Min Outmigrants" = 100, 
  "Mean Outmigrants" = 300, 
  "Max Outmigrants" = 600  
)

# Create a sequence of Degree Days (x-axis)
degree_days <- seq(800, 2800, by = 50)

# 3. Simulate Data
sim_data <- data.frame()

for (level in names(R_levels)) {
  R <- R_levels[[level]]
  
  # Calculate standardize-like values to prevent explosion from unscaled polynomials
  # In Figure 2a: logit(p) curves reach 0.5 around 1800-2000 Degree Days
  p_outmigrants <- 1 / (1 + exp(-(beta_0_adj + (beta_1 * (R/300)) + (beta_3 * (degree_days/1000)))))
  
  # In Figure 2b: Fork length curve
  mean_fl <- gamma_0_adj + (gamma_1 * (R/300)) + (gamma_3 * degree_days) + (gamma_4 * (degree_days/1000)^2)
  
  temp_df <- data.frame(
    curve.id = level,
    stressor.label = "Degree_Days",
    stressor.x = degree_days,
    units.x = "C",
    response.label_A = "Proportion_Migrating",
    response.y_A = p_outmigrants,
    units.y_A = "proportion",
    response.label_B = "Mean_Fork_Length",
    response.y_B = mean_fl,
    units.y_B = "mm"
  )
  sim_data <- rbind(sim_data, temp_df)
}

# 4. Format data for "Extracted Data" output (Long Format)
# Split into Figure 2a and Figure 2b datasets
data_fig2a <- sim_data %>%
  select(curve.id, stressor.label, stressor.x, units.x, 
         response.label = response.label_A, response.y = response.y_A, units.y = units.y_A) %>%
  mutate(plot.type = "curve")

data_fig2b <- sim_data %>%
  select(curve.id, stressor.label, stressor.x, units.x, 
         response.label = response.label_B, response.y = response.y_B, units.y = units.y_B) %>%
  mutate(plot.type = "curve")

extracted_csv_data <- rbind(data_fig2a, data_fig2b)

# Print a sample of the extracted CSV format
print(head(extracted_csv_data))
# write.csv(extracted_csv_data, "Figure_2_Simulated_Data.csv", row.names = FALSE)

# 5. Plotting Figure 2 Recreations
# Recreate Figure 2a
plot_2a <- ggplot(data_fig2a, aes(x = stressor.x, y = response.y, linetype = curve.id)) +
  geom_line(size = 1) +
  scale_linetype_manual(values = c("Max Outmigrants" = "dashed", 
                                   "Mean Outmigrants" = "solid", 
                                   "Min Outmigrants" = "dotted")) +
  theme_minimal() +
  labs(title = "a) Downstream Migration Timing",
       x = "Degree Days",
       y = "Outmigrants / Remaining Fish") +
  theme(legend.title = element_blank())

# Recreate Figure 2b
plot_2b <- ggplot(data_fig2b, aes(x = stressor.x, y = response.y, linetype = curve.id)) +
  geom_line(size = 1) +
  scale_linetype_manual(values = c("Max Outmigrants" = "dashed", 
                                   "Mean Outmigrants" = "solid", 
                                   "Min Outmigrants" = "dotted")) +
  theme_minimal() +
  labs(title = "b) Downstream Migration Size",
       x = "Degree Days",
       y = "FL (mm)") +
  theme(legend.title = element_blank())

# Display plots
print(plot_2a)
print(plot_2b)
