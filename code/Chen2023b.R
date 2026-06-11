# Chen, E. K., Som, N. A., Deibner-Hanson, J. D., Anderson, D. G., & Henderson, M. J. (2023). A life cycle model for evaluating 
# estuary residency and recovery potential in Chinook salmon. Fisheries Research, 257, 106511.

# Code generated with Gemini 3.1 Pro on 6/11/26 using a custom GEM 'SRF Data Extraction' by AH Fullerton


# Load required libraries
library(ggplot2)
library(dplyr)

# 1. Define extracted parameters
# Estuary Residents
A_est <- 0.181
x_est <- 92.8
c_est <- 1.00

# Ocean Migrants
A_oc <- 0.098
x_oc <- 73.0
c_oc <- 0.353

# 2. Define the ranges for Fork Length (FL) based on Figure 4 x-axes
fl_est <- seq(80, 120, by = 0.5)
fl_oc <- seq(50, 90, by = 0.5)

# 3. Simulate Data using the logistic equations
# Calculate expected survival for Estuary Residents
surv_est <- A_est / (1 + exp(c_est * (x_est - fl_est)))

# Calculate expected survival for Ocean Migrants
surv_oc <- A_oc / (1 + exp(c_oc * (x_oc - fl_oc)))

# 4. Format data for "Extracted Data" output (Long Format)
data_est <- data.frame(
  curve.id = "Estuary_Residents",
  stressor.label = "Mean_Fork_Length",
  stressor.x = fl_est,
  units.x = "mm",
  response.label = "First_Year_Ocean_Survival",
  response.y = surv_est,
  units.y = "proportion",
  plot.type = "curve"
)

data_oc <- data.frame(
  curve.id = "Ocean_Migrants",
  stressor.label = "Mean_Fork_Length",
  stressor.x = fl_oc,
  units.x = "mm",
  response.label = "First_Year_Ocean_Survival",
  response.y = surv_oc,
  units.y = "proportion",
  plot.type = "curve"
)

# Combine datasets
extracted_fig4_data <- rbind(data_est, data_oc)

# Print a sample of the formatted data
print(head(extracted_fig4_data))
# write.csv(extracted_fig4_data, "Figure_4_Simulated_Data.csv", row.names = FALSE)

# 5. Plotting Figure 4 Recreations
# Recreate Figure 4 (Left) - Estuary Residents
plot_est <- ggplot(data_est, aes(x = stressor.x, y = response.y)) +
  geom_line(size = 1) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 0.35), breaks = seq(0, 0.3, by = 0.1)) +
  labs(title = "Estuary Residents",
       x = "Mean Estuary Resident Fork Length (mm)",
       y = "First-year Ocean Survival") +
  annotate("text", x = 100, y = 0.30, 
           label = "S['1.est'] == 0.181 / (1 + e^{1.00*(92.8 - FL[est])})", parse = TRUE)

# Recreate Figure 4 (Right) - Ocean Migrants
plot_oc <- ggplot(data_oc, aes(x = stressor.x, y = response.y)) +
  geom_line(size = 1) +
  theme_classic() +
  scale_y_continuous(limits = c(0, 0.35), breaks = seq(0, 0.3, by = 0.1)) +
  labs(title = "Ocean Migrants",
       x = "Mean Ocean Migrant Fork Length (mm)",
       y = "First-year Ocean Survival") +
  annotate("text", x = 70, y = 0.30, 
           label = "S['1.oc'] == 0.098 / (1 + e^{0.353*(73.0 - FL[oc])})", parse = TRUE)

# Display plots
print(plot_est)
print(plot_oc)
