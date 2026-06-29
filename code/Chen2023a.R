# Chen, E. K., Som, N. A., Deibner-Hanson, J. D., Anderson, D. G., & Henderson, M. J. (2023). A life cycle model for evaluating 
# estuary residency and recovery potential in Chinook salmon. Fisheries Research, 257, 106511.

# Code generated with Gemini 3.1 Pro on 6/11/26 using a custom GEM 'SRF Data Extraction' by AH Fullerton

# Version 1 (simulate from equation and coefficients given): ####
# Load required libraries
library(ggplot2)
library(dplyr)

# 1. Define known parameters and assumed intercepts for Figure 2a
beta_1 <- 0.396
beta_3 <- 2.270
beta_0_adj <- -5.5 # Assumed intercept to match the visual position in Figure 2a

# Define standard total outmigrant abundances (R)
R_levels <- list(
  "Min Outmigrants" = 100, 
  "Mean Outmigrants" = 300, 
  "Max Outmigrants" = 600  
)

# Sequence for the expectation curves (Lines)
degree_days_seq <- seq(800, 2800, by = 50)

# 2. Setup Dataframes
data_fig2a_lines <- data.frame()
data_fig2a_pts <- data.frame()

set.seed(123) # For reproducibility
n_pts <- 40   # Number of simulated data points per curve

# 3. Simulate Data (Lines and Points)
for (level in names(R_levels)) {
  R <- R_levels[[level]]
  
  # --- Generate Expected Mean Lines ---
  # logit(p) = B0 + B1*(R_scaled) + B3*(DD_scaled)
  logit_mean <- beta_0_adj + (beta_1 * (R/300)) + (beta_3 * (degree_days_seq/1000))
  p_mean <- 1 / (1 + exp(-logit_mean))
  
  temp_line <- data.frame(
    curve.id = level,
    stressor.x = degree_days_seq,
    response.y = p_mean
  )
  data_fig2a_lines <- rbind(data_fig2a_lines, temp_line)
  
  # --- Generate Simulated Data Points with Error ---
  dd_pts <- runif(n_pts, 800, 2800)
  
  # Add process error to the logit scale
  error <- rnorm(n_pts, mean = 0, sd = 0.5) 
  logit_pts <- beta_0_adj + (beta_1 * (R/300)) + (beta_3 * (dd_pts/1000)) + error
  p_pts <- 1 / (1 + exp(-logit_pts))
  
  temp_pts <- data.frame(
    curve.id = level,
    stressor.label = "Degree_Days",
    stressor.x = dd_pts,
    units.x = "C",
    response.label = "Proportion_Migrating",
    response.y = p_pts,
    units.y = "proportion",
    plot.type = "scatter"
  )
  data_fig2a_pts <- rbind(data_fig2a_pts, temp_pts)
}

# 4. Format data for "Extracted Data" output (Combining lines format if needed, but focusing on points for CSV)
# write.csv(data_fig2a_pts, "Figure_2a_Simulated_Points.csv", row.names = FALSE)
print(head(data_fig2a_pts))

# 5. Plotting Figure 2a Recreation (Points + Lines)
plot_2a <- ggplot() +
  # Simulated data points in gray, mapped by shape to differentiate cohorts if desired
  geom_point(data = data_fig2a_pts, aes(x = stressor.x, y = response.y, shape = curve.id), 
             color = "gray50", size = 2, alpha = 0.6) +
  # Mean expectation lines
  geom_line(data = data_fig2a_lines, aes(x = stressor.x, y = response.y, linetype = curve.id), 
            size = 1, color = "black") +
  scale_linetype_manual(values = c("Max Outmigrants" = "dashed", 
                                   "Mean Outmigrants" = "solid", 
                                   "Min Outmigrants" = "dotted")) +
  theme_classic() +
  labs(title = "a) Downstream Migration Timing",
       x = "Degree Days",
       y = "Outmigrants / Remaining Fish") +
  theme(legend.title = element_blank(),
        legend.position = "bottom")

# Display plot
print(plot_2a)




# Version 2 (fit to digitized data) ####
# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)

# 1. Load the digitized data
# Replace 'Default Dataset (20).csv' with your actual file path if needed
digitized_data <- read_csv("data/Chen2023a_data.csv", show_col_types = FALSE)

# Rename columns to be descriptive
colnames(digitized_data) <- c("Degree_Days", "Proportion")

# Constrain proportions to [0, 1] to fix any minor digitizing overshoots/undershoots
digitized_data$Proportion <- pmax(0, pmin(1, digitized_data$Proportion))

# 2. Refit the curve using a quasibinomial Generalized Linear Model (GLM)
# This models the logistic shape characteristic of Figure 2a
fit <- glm(Proportion ~ Degree_Days, 
           data = digitized_data, 
           family = quasibinomial(link = "logit"))

# Print a summary of the refitted model parameters
print(summary(fit))

# 3. Generate expected line data from the fit
# Create a smooth sequence of Degree Days based on the x-axis range
x_seq <- seq(min(digitized_data$Degree_Days), max(digitized_data$Degree_Days), length.out = 200)

# Predict the mean proportion for the smooth sequence
y_pred <- predict(fit, newdata = data.frame(Degree_Days = x_seq), type = "response")

data_line <- data.frame(
  stressor.x = x_seq,
  response.y = y_pred
)

# 4. Format data for "Extracted Data" output
# Format the scatter points
extracted_pts <- data.frame(
  curve.id = "Digitized_Cohort",
  stressor.label = "Degree_Days",
  stressor.x = digitized_data$Degree_Days,
  units.x = "C",
  response.label = "Proportion_Migrating",
  response.y = digitized_data$Proportion,
  units.y = "proportion",
  plot.type = "scatter"
)

# Format the refitted curve
extracted_line <- data.frame(
  curve.id = "Refitted_Curve",
  stressor.label = "Degree_Days",
  stressor.x = data_line$stressor.x,
  units.x = "C",
  response.label = "Proportion_Migrating",
  response.y = data_line$response.y,
  units.y = "proportion",
  plot.type = "curve"
)

# Combine for final extracted output and display
final_extracted_data <- rbind(extracted_pts, extracted_line)
print(head(final_extracted_data))
# write.csv(final_extracted_data, "Figure_2a_Refitted_Data.csv", row.names = FALSE)

# 5. Plot the Digitized Points and the Refitted Curve
plot_2a_refit <- ggplot() +
  # Hand-digitized points
  geom_point(data = extracted_pts, aes(x = stressor.x, y = response.y), 
             color = "gray50", size = 2, alpha = 0.6) +
  # Refitted expectation curve
  geom_line(data = extracted_line, aes(x = stressor.x, y = response.y), 
            size = 1.2, color = "black") +
  theme_classic() +
  labs(title = "a) Downstream Migration Timing (Refitted)",
       subtitle = "Black line represents the quasibinomial GLM fit to digitized points",
       x = "Degree Days",
       y = "Outmigrants / Remaining Fish") +
  theme(legend.position = "none")

# Display plot
print(plot_2a_refit)