# Anderson, J. J., Beer, W. N., Israel, J. A., & Greene, S. (2022). Targeting river 
# operations to the critical thermal window of fish incubation: Model and case study on 
# Sacramento River winter‐run Chinook salmon. River Research and Applications, 38(5), 895–905.

# Anderson et al. 2022: Chinook egg-to-fry survival vs critical window temperature

# Load required library for plotting
library(ggplot2)

# Define the temperature sequence (10 to 15 degrees Celsius)
temps <- seq(10, 15, by = 0.5)

# Function to calculate thermal survival over a constant temperature window
# Formula derived from Anderson et al. 2022: V_i = exp(-b_delta * sum(Delta_y_i))
# where Delta_y_i = max(T - T_crit, 0)
# Assuming constant temperature over the window duration (delta), the sum is delta * Delta_y_i
calc_survival <- function(T, T_crit, b_delta, delta) {
  # Calculate degrees above the critical threshold
  temp_diff <- pmax(T - T_crit, 0)
  
  # Calculate sum of differences over the window delta
  sum_diff <- delta * temp_diff
  
  # Calculate final survival proportion
  V <- exp(-b_delta * sum_diff)
  return(V)
}

# Define model parameters extracted from Anderson et al. 2022
# Parameters for O-type CW (organogenesis prior to hatching)
T_crit_O <- 11.82
b_delta_O <- 0.4361
delta_O <- 4

# Parameters for E-type CW (entire incubation period)
T_crit_E <- 12.04
b_delta_E <- 0.030
delta_E <- 75

# Calculate survival for both CW parameterizations
surv_O <- calc_survival(temps, T_crit_O, b_delta_O, delta_O)
surv_E <- calc_survival(temps, T_crit_E, b_delta_E, delta_E)

# Create the data frame
df_O <- data.frame(
  curve.id = rep("O_type", length(temps)),
  stressor.label = rep("temperature", length(temps)),
  stressor.x = temps,
  units.x = rep("degC", length(temps)),
  response.label = rep("thermal_survival", length(temps)),
  response.y = round(surv_O, 4),
  units.y = rep("proportion", length(temps)),
  stressor.value = rep("4_day_CW", length(temps))
)

df_E <- data.frame(
  curve.id = rep("E_type", length(temps)),
  stressor.label = rep("temperature", length(temps)),
  stressor.x = temps,
  units.x = rep("degC", length(temps)),
  response.label = rep("thermal_survival", length(temps)),
  response.y = round(surv_E, 4),
  units.y = rep("proportion", length(temps)),
  stressor.value = rep("75_day_CW", length(temps))
)

# Combine into a single final dataset
final_df <- rbind(df_O, df_E)

# View the formatted data in the console
print(final_df)

# Save the dataset to a CSV file
write.csv(final_df, "Anderson_et_al_2022_SRF_data.csv", row.names = FALSE)

# Generate a plot comparing the two curves
p <- ggplot(final_df, aes(x = stressor.x, y = response.y, color = curve.id)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("O_type" = "darkred", "E_type" = "steelblue")) +
  labs(
    title = "Thermal Survival vs. Incubation Temperature",
    subtitle = "Extracted from Anderson et al. 2022 (Sacramento River Winter-run Chinook)",
    x = "Temperature (°C)",
    y = "Thermal Survival (Proportion)",
    color = "Curve ID"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

# Display the plot
print(p)

# Save the plot as a PNG image
ggsave("Anderson_et_al_2022_SRF_plot.png", plot = p, width = 8, height = 5, dpi = 300)
