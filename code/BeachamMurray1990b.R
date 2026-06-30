# -------------------------------------------------------------------------
# Beacham, T. D., & Murray, C. B. (1990). Temperature, egg size, and development of embryos and alevins of five species of 
# Pacific salmon: a comparative analysis. Transactions of the American Fisheries Society, 119(6), 927–945.
# Beacham and Murray 1990: Pacific salmon emergence time vs incubation temperature
# Model 4: log_e(D) = log_e(a) + b * log_e(T - c)
# -------------------------------------------------------------------------


# install packages
library(ggplot2)

# 1. Define the species-specific parameters from Table A.3
params <- data.frame(
  species = c("Pink (Odd-Year)", "Pink (Even-Year)", "Chum", 
              "Chinook", "Coho", "Sockeye"),
  curve_id = c("pink_odd_emerge", "pink_even_emerge", "chum_emerge", 
               "chinook_emerge", "coho_emerge", "sockeye_emerge"),
  log_a = c(6.233, 7.231, 6.307, 10.404, 7.018, 7.647),
  b = c(-0.693, -0.967, -0.764, -2.043, -1.069, -1.134),
  c = c(0.153, -3.446, -0.039, -7.575, -2.062, -3.514)
)

# 2. Define the evaluation temperatures (in degrees Celsius)
temps <- c(2, 4, 8, 12, 15)

# 3. Create an empty list to store the generated data rows
results <- list()

# 4. Iterate through each species and temperature to calculate emergence time
for (i in 1:nrow(params)) {
  for (t in temps) {
    
    # Calculate emergence time (D)
    # The math transforms the log equation: D = exp(log_a + b * log(T - c))
    D <- exp(params$log_a[i] + params$b[i] * log(t - params$c[i]))
    
    # Round to the nearest whole day
    D_rounded <- round(D)
    
    # Append the formatted row to the results list
    results[[length(results) + 1]] <- data.frame(
      curve.id = params$curve_id[i],
      stressor.label = "water_temperature",
      stressor.x = t,
      units.x = "degC",
      response.label = "emergence_time",
      response.y = D_rounded,
      units.y = "days",
      stressor.value = params$species[i]
    )
  }
}

# 5. Combine the list of rows into a single data frame
final_csv_data <- do.call(rbind, results)

# 6. Display the final dataset in the console
print(final_csv_data)


# 7. Export directly to a CSV file in your working directory
write.csv(final_csv_data, "beacham_murray_1990_emergence_time.csv", row.names = FALSE)

# 8. Generate the plot mapping temperature to emergence time, colored by species
emergence_plot <- ggplot(final_csv_data, aes(x = stressor.x, y = response.y, 
                                             color = stressor.value, group = curve.id)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = temps) + 
  scale_y_continuous(breaks = seq(0, 350, by = 50)) +
  labs(
    title = "Pacific Salmon Emergence Time vs. Incubation Temperature",
    subtitle = "Derived from Beacham & Murray (1990) Model 4",
    x = "Incubation Temperature (°C)",
    y = "Time to Emergence (Days)",
    color = "Species / Broodline"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

# Display the plot
print(emergence_plot)
