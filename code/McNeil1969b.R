# McNeil, W. J. (1969). Survival of pink and chum salmon eggs and alevins. 
# Salmon and trout in streams. HR McMillan Lectures in Fisheries, University of British Columbia, Vancouver, 101-117.
# McNeil 1969: Chum salmon freshwater survival vs. spawning time
# equation: y = 10.14-0.24x (Figure 4)

# Load necessary library for plotting
library(ggplot2)

# 1. Generate Data
x_days <- seq(0, 40, by = 5) # Days past August 10
y_survival <- 10.14 - (0.24 * x_days) # Chum salmon equation

y_survival <- ifelse(y_survival < 0, 0, y_survival)

# Create dataframe
srf3_data <- data.frame(
  curve.id = rep("McNeil_1969_Fig4", length(x_days)),
  stressor.label = rep("spawning_timing", length(x_days)),
  stressor.x = x_days,
  units.x = rep("days_after_august_10", length(x_days)),
  response.label = rep("freshwater_survival", length(x_days)),
  response.y = round(y_survival, 2),
  units.y = rep("percent", length(x_days))
)

# 2. Export to CSV
write.csv(srf3_data, "McNeil_1969_ChumSalmon.csv", row.names = FALSE)

# 3. Generate Plot
ggplot(srf3_data, aes(x = stressor.x, y = response.y)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(size = 2, color = "green4") +
  theme_minimal() +
  labs(
    title = "Chum Salmon Freshwater Survival vs. Spawning Time",
    x = "Spawning Timing (Days after August 10)",
    y = "Freshwater Survival (%)"
  )
