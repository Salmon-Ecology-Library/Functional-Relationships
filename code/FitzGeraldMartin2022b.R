# FitzGerald, A. M., & Martin, B. T. (2022). Quantification of thermal impacts across 
# freshwater life stages to improve temperature management for anadromous salmonids. 
# Conservation Physiology, 10(1), coac013.
# FitzGerald et al. 2022: Chinook egg-to-fry survival vs incubation temperature

# Load libraries
library(ggplot2)

# Parameters for embryonic mortality
Tcrit <- 12.0
st <- 0.024

# Function to calculate overall survival over n days of incubation
calc_embryo_survival <- function(temp, days) {
  if (temp <= Tcrit) {
    return(1.0) # 100% survival if below critical threshold
  } else {
    # Calculate mortality rate and convert to survival proportion
    mortality <- 1 - exp(-st * (temp - Tcrit) * days)
    survival <- 1 - mortality
    return(max(0, survival)) # ensure it doesn't drop below 0
  }
}

# Define stressor ranges and curve groupings
temperatures <- seq(5, 25, by = 1)
incubation_days <- c(25, 50, 75, 100, 125, 150, 175)

results <- list()

for (days in incubation_days) {
  for (temp in temperatures) {
    survival_prop <- calc_embryo_survival(temp, days)
    
    # Append to results matching the CSV template
    results[[length(results) + 1]] <- data.frame(
      curve.id = paste0("c_embryo_", days, "d"),
      stressor.label = "temperature",
      stressor.x = temp,
      units.x = "degC",
      response.label = "survival",
      response.y = round(survival_prop, 3),
      units.y = "proportion",
      stressor.value = paste0(days, "d"),
      stringsAsFactors = FALSE
    )
  }
}

# Combine and write to CSV
srf2_data <- do.call(rbind, results)
write.csv(srf2_data, "SRF2_Embryonic_Incubation_Data.csv", row.names = FALSE)

# Convert stressor.value to a factor to keep the legend in numerical order
srf2_data$stressor.value <- factor(srf2_data$stressor.value, 
                                   levels = paste0(incubation_days, "d"))

# Plot the data
srf2_plot <- ggplot(srf2_data, aes(x = stressor.x, y = response.y, color = stressor.value)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_vline(xintercept = 12.0, linetype = "dashed", color = "black") + # Tcrit threshold
  labs(
    title = "Embryo Egg-to-Fry Survival vs. Temperature (FitzGerald et al. 2022)",
    subtitle = "Dashed line indicates Tcrit (12°C)",
    x = "Temperature (°C)",
    y = "Survival (Proportion)",
    color = "Incubation Duration"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

print(srf2_plot)
