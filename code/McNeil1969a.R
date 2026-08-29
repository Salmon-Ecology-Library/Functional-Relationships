# McNeil, W. J. (1969). Survival of pink and chum salmon eggs and alevins. 
# Salmon and trout in streams. HR McMillan Lectures in Fisheries, University of British Columbia, Vancouver, 101-117.
# McNeil 1969: Pink salmon freshwater survival vs. spawning time
# equation: y = 26.66-0.72x (Figure 3)

# Load necessary library for plotting
library(ggplot2)

# 1. Generate Data
x_days <- seq(0, 40, by = 5) # Days past August 10
y_survival <- 26.66 - (0.72 * x_days) # Pink salmon equation

# Ensure survival doesn't drop below 0 logically (though the linear model eventually crosses 0)
y_survival <- ifelse(y_survival < 0, 0, y_survival)

# Create dataframe
srf2_data <- data.frame(
  curve.id = rep("McNeil_1969_Fig3", length(x_days)),
  stressor.label = rep("spawning_timing", length(x_days)),
  stressor.x = x_days,
  units.x = rep("days_after_august_10", length(x_days)),
  response.label = rep("freshwater_survival", length(x_days)),
  response.y = round(y_survival, 2),
  units.y = rep("percent", length(x_days))
)

# 2. Export to CSV
write.csv(srf2_data, "McNeil_1969_SRF2_PinkSalmon.csv", row.names = FALSE)

# 3. Generate Plot
ggplot(srf2_data, aes(x = stressor.x, y = response.y)) +
  geom_line(color = "blue", size = 1) +
  geom_point(size = 2, color = "darkblue") +
  theme_minimal() +
  labs(
    title = "Pink Salmon Freshwater Survival vs. Spawning Time",
    x = "Spawning Timing (Days after August 10)",
    y = "Freshwater Survival (%)"
  )

