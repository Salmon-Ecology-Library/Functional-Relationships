# Beamer, E., Hayman, B., Hinton, S., & Skagit River System Cooperative. (2005). 
# LINKING WATERSHED CONDITIONS TO EGG-TO-FRY SURVIVAL OF SKAGIT CHINOOK SALMON. 
# In An Appendix to the Skagit River System Cooperative Chinook Recovery Plan. Skagit River System Cooperative.
# Beamer et al. 2005: Chinook egg-to-migrant-fry survival vs. flood recurrence interval
# Equation 2


# Load libraries
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# 1. Define the parameters and equation for SRF 2 (Beamer et al. 2005)
# Equation: S_T = 0.180 * exp(-0.035 * RI)
intercept <- 0.180
decay_rate <- -0.035

# Generate a sequence of Flood Recurrence Interval (RI) values (years)
# We use specific intervals to populate the dataset
ri_values <- c(2, 5, 10, 20, 50, 100)

# Calculate the corresponding egg-to-migrant-fry survival (S_T)
survival_values <- intercept * exp(decay_rate * ri_values)

# 2. Format the data
srf2_data <- data.frame(
  curve.id = rep("c1", length(ri_values)),
  stressor.label = rep("flood_recurrence_interval", length(ri_values)),
  stressor.x = ri_values,
  units.x = rep("years", length(ri_values)),
  response.label = rep("survival", length(ri_values)),
  response.y = round(survival_values, 4), # Rounded for clean CSV output
  units.y = rep("proportion", length(ri_values))
)

# 3. Export to CSV
# This file can be directly copied into your "Stressor-Response e-Library Metadata and Data Template.xlsx"
write.csv(srf2_data, "Beamer2005_SRF2_Data.csv", row.names = FALSE)
cat("CSV successfully generated: Beamer2005_SRF2_Data.csv\n")

# 4. Generate the Plot
# Create a higher-resolution sequence for a smooth plot line
plot_ri <- seq(0, 100, length.out = 200)
plot_survival <- intercept * exp(decay_rate * plot_ri)
plot_df <- data.frame(RI = plot_ri, Survival = plot_survival)

# Plot using ggplot2
srf_plot <- ggplot() +
  # Add the smooth exponential curve
  geom_line(data = plot_df, aes(x = RI, y = Survival), color = "#0073C2", size = 1.2) +
  # Overlay the specific extracted data points
  geom_point(data = srf2_data, aes(x = stressor.x, y = response.y), color = "red", size = 3) +
  labs(
    title = "SRF 2: Egg-to-Migrant-Fry Survival vs. Flood Recurrence (Equation 2)",
    x = "Flood Recurrence Interval (years)",
    y = "Egg-to-Migrant-Fry Survival (proportion)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Display the plot
print(srf_plot)

# Optional: Save the plot as an image file
# ggsave("Beamer2005_SRF2_Plot.png", plot = srf_plot, width = 8, height = 6, dpi = 300)
