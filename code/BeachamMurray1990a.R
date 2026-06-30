# -------------------------------------------------------------------------
# Beacham, T. D., & Murray, C. B. (1990). Temperature, egg size, and development of embryos and alevins of five species of 
# Pacific salmon: a comparative analysis. Transactions of the American Fisheries Society, 119(6), 927–945.
# Beacham and Murray 1990: Pacific salmon hatching time vs incubation temperature
# Model 4: log_e(D) = log_e(a) + b * log_e(T - c)
# -------------------------------------------------------------------------

# Install packages
library(ggplot2)

# 1. Define the species-specific parameters from Table A.2
# Note: Sockeye 'b' and 'c' parameters are swapped here to correct the 
# typo in the original publication text.
params <- data.frame(
  species = c("Pink (Odd-Year)", "Pink (Even-Year)", "Chum", 
              "Chinook", "Coho", "Sockeye"),
  curve_id = c("pink_odd_hatch", "pink_even_hatch", "chum_hatch", 
               "chinook_hatch", "coho_hatch", "sockeye_hatch"),
  log_a = c(7.962, 6.375, 7.049, 7.192, 7.889, 8.734),
  b = c(-1.382, -0.885, -1.209, -1.292, -1.536, -1.589),
  c = c(-5.942, -1.850, -2.407, -2.056, -4.085, -7.067)
)

# 2. Define the evaluation temperatures (in degrees Celsius)
temps <- c(2, 4, 8, 12, 15)

# 3. Create an empty list to store the generated data rows
results <- list()

# 4. Iterate through each species and temperature to calculate hatching time
for (i in 1:nrow(params)) {
  for (t in temps) {
    
    # Calculate hatching time (D)
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
      response.label = "hatching_time",
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
write.csv(final_csv_data, "beacham_murray_1990_hatching_time.csv", row.names = FALSE)

# 8. Generate the plot mapping temperature to hatching time, colored by species
hatching_plot <- ggplot(final_csv_data, aes(x = stressor.x, y = response.y, 
                                            color = stressor.value, group = curve.id)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = temps) + 
  scale_y_continuous(breaks = seq(0, 250, by = 25)) +
  labs(
    title = "Pacific Salmon Hatching Time vs. Incubation Temperature",
    subtitle = "Derived from Beacham & Murray (1990) Model 4",
    x = "Incubation Temperature (°C)",
    y = "Time to Hatch (Days)",
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
print(hatching_plot)

