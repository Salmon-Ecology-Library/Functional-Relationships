# FitzGerald, A. M., & Martin, B. T. (2022). Quantification of thermal impacts across 
# freshwater life stages to improve temperature management for anadromous salmonids. 
# Conservation Physiology, 10(1), coac013.
# FitzGerald et al. 2022: Chinook pre-spawn holding energy expenditure vs temperature

# Load libraries
library(ggplot2)

# Parameters for Chinook salmon holding
c0 <- 1565.04
M <- 7.37
b <- -0.217
D <- 0.068
conv_factor <- 1.358442e-5 # converts mgO2 to MJ

# Function to calculate daily energy expenditure
calc_daily_energy <- function(temp) {
  daily_mgO2 <- c0 * (M^b) * exp(D * temp)
  daily_MJ <- daily_mgO2 * conv_factor
  return(daily_MJ)
}

# Define stressor ranges and curve groupings
temperatures <- seq(5, 25, by = 1) 
holding_days <- c(25, 50, 75, 100, 125, 150, 175)

# Initialize empty list to store rows
results <- list()

for (days in holding_days) {
  for (temp in temperatures) {
    daily_energy <- calc_daily_energy(temp)
    total_energy <- daily_energy * days
    
    # Append to results matching the CSV template
    results[[length(results) + 1]] <- data.frame(
      curve.id = paste0("c_adult_", days, "d"),
      stressor.label = "temperature",
      stressor.x = temp,
      units.x = "degC",
      response.label = "energy_use",
      response.y = round(total_energy, 3),
      units.y = "MJ/kg",
      stressor.value = paste0(days, "d"),
      stringsAsFactors = FALSE
    )
  }
}

# Combine and write to CSV
srf1_data <- do.call(rbind, results)
write.csv(srf1_data, "SRF1_Adult_Holding_Data.csv", row.names = FALSE)

# Convert stressor.value to a factor to keep the legend in numerical order
srf1_data$stressor.value <- factor(srf1_data$stressor.value, 
                                   levels = paste0(holding_days, "d"))

# Plot the data
srf1_plot <- ggplot(srf1_data, aes(x = stressor.x, y = response.y, color = stressor.value)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "Adult Chinook Salmon Pre-Spawn Holding Costs",
    subtitle = "Energy expenditure increases exponentially with temperature",
    x = "Temperature (°C)",
    y = "Energy Use (MJ/kg)",
    color = "Holding Duration"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

print(srf1_plot)
