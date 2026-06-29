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

# 2. Define the ranges for Fork Length (FL) for the expectation curves (Lines)
fl_est_seq <- seq(80, 120, by = 0.5)
fl_oc_seq <- seq(50, 90, by = 0.5)

# Calculate expected mean survival (Error = 0)
surv_est_line <- A_est / (1 + exp(c_est * (x_est - fl_est_seq)))
surv_oc_line <- A_oc / (1 + exp(c_oc * (x_oc - fl_oc_seq)))

data_est_line <- data.frame(stressor.x = fl_est_seq, response.y = surv_est_line)
data_oc_line <- data.frame(stressor.x = fl_oc_seq, response.y = surv_oc_line)

# 3. Simulate "observed" data points incorporating the error term (epsilon)
set.seed(42) # For reproducibility
n_pts <- 60  # Number of simulated cohorts/observations

# Estuary Residents simulated points
fl_est_pts <- runif(n_pts, 80, 120)
eps_est <- rnorm(n_pts, mean = 0, sd = 8) # Simulated process error for epsilon
surv_est_pts <- A_est / (1 + exp(c_est * (x_est - fl_est_pts + eps_est)))

data_est_pts <- data.frame(
  curve.id = "Estuary_Residents", stressor.label = "Mean_Fork_Length",
  stressor.x = fl_est_pts, units.x = "mm", response.label = "First_Year_Ocean_Survival",
  response.y = surv_est_pts, units.y = "proportion", plot.type = "scatter"
)

# Ocean Migrants simulated points
fl_oc_pts <- runif(n_pts, 50, 90)
eps_oc <- rnorm(n_pts, mean = 0, sd = 8) # Simulated process error for epsilon
surv_oc_pts <- A_oc / (1 + exp(c_oc * (x_oc - fl_oc_pts + eps_oc)))

data_oc_pts <- data.frame(
  curve.id = "Ocean_Migrants", stressor.label = "Mean_Fork_Length",
  stressor.x = fl_oc_pts, units.x = "mm", response.label = "First_Year_Ocean_Survival",
  response.y = surv_oc_pts, units.y = "proportion", plot.type = "scatter"
)

# Combine datasets for extracted output
extracted_fig4_data <- rbind(data_est_pts, data_oc_pts)
data_est_pts <- data.frame(
  curve.id = "Estuary_Residents", stressor.label = "Mean_Fork_Length",
  stressor.x = fl_est_pts, units.x = "mm", response.label = "First_Year_Ocean_Survival",
  response.y = surv_est_pts, units.y = "proportion", plot.type = "scatter"
)

data2save <- rbind(cbind(data_est_line, "curve.id" = "Estuary_Residents"), cbind(data_oc_line, "curve.id" = "First_Year_Ocean"))
data2save <- cbind(data2save, "stressor.label" = "Mean_Fork_Length", "units.x" = "mm", "response.label" =
                           "First_year_ocean_survival", "units.y" = "proportion", "plot.type" = "curve")
data2save <- data2save[, c("curve.id", "stressor.label", "stressor.x", "units.x", "response.label", "response.y", "units.y", "plot.type")]
write.csv(data2save, "data/Chen2023b_data.csv", row.names = FALSE)

# 4. Plotting Figure 4 Recreations (Points + Line)
# Recreate Figure 4 (Left) - Estuary Residents
plot_est <- ggplot() +
  geom_point(data = data_est_pts, aes(x = stressor.x, y = response.y), 
             color = "gray60", size = 2, alpha = 0.7) + # Simulated data points in gray
  geom_line(data = data_est_line, aes(x = stressor.x, y = response.y), 
            size = 1, color = "black") +                # Mean expectation line
  theme_classic() +
  scale_y_continuous(limits = c(0, 0.35), breaks = seq(0, 0.3, by = 0.1)) +
  labs(title = "Estuary Residents",
       x = "Mean Estuary Resident Fork Length (mm)",
       y = "First-year Ocean Survival") +
  annotate("text", x = 100, y = 0.30, 
           label = "S['1.est'] == 0.181 / (1 + e^{1.00*(92.8 - FL[est])})", parse = TRUE)

# Recreate Figure 4 (Right) - Ocean Migrants
plot_oc <- ggplot() +
  geom_point(data = data_oc_pts, aes(x = stressor.x, y = response.y), 
             color = "gray60", size = 2, alpha = 0.7) + # Simulated data points in gray
  geom_line(data = data_oc_line, aes(x = stressor.x, y = response.y), 
            size = 1, color = "black") +                # Mean expectation line
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
