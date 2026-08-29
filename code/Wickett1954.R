# Wickett, W. P. (1954). The Oxygen Supply to Salmon Eggs in Spawning Beds. 
# J. Fish. Res. Bd. Can., 11(6), 933-953.
# Wickett 1954: Apparent velocity vs limiting dissolved oxygen for pre-eyed chum salmon eggs
# Equation 5


# Load necessary library for plotting
library(ggplot2)

# 1. Define stressor.x values (apparent velocity in mm/hr)
# Generating a sequence from 5 to 100 to capture the curve represented in Figure 6
# Using 100 points for a smooth plot line
v_values <- seq(5, 100, length.out = 100)

# 2. Calculate response.y values (limiting dissolved oxygen in ppm)
# Based on the reduced equations: v(DO - 1.67) = 5.5 (for n=1) and v(DO - 1.67) = 55 (for n=10)
do_n1 <- (5.5 / v_values) + 1.67
do_n10 <- (55 / v_values) + 1.67

# 3. Create data frame for n=1 curve
df_n1 <- data.frame(
  curve.id = "n1_single_egg",
  stressor.label = "apparent_velocity",
  stressor.x = v_values,
  units.x = "mm/hr",
  response.label = "limiting_dissolved_oxygen",
  response.y = round(do_n1, 3),
  units.y = "ppm",
  stressor.value = "n=1"
)

# 4. Create data frame for n=10 curve
df_n10 <- data.frame(
  curve.id = "n10_ten_eggs",
  stressor.label = "apparent_velocity",
  stressor.x = v_values,
  units.x = "mm/hr",
  response.label = "limiting_dissolved_oxygen",
  response.y = round(do_n10, 3),
  units.y = "ppm",
  stressor.value = "n=10"
)

# 5. Combine into a single data frame
srf2_data <- rbind(df_n1, df_n10)

# 6. Export to CSV
write.csv(srf2_data, "Wickett_1954_SRF2_Data.csv", row.names = FALSE)

# 7. Plot the curves using ggplot2
srf2_plot <- ggplot(srf2_data, aes(x = stressor.x, y = response.y, color = stressor.value)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(0, 15))
  labs(
    title = "Apparent Velocity vs. Limiting Dissolved Oxygen",
    subtitle = "Minimum requirements for pre-eyed chum salmon eggs at 8°C (Wickett 1954)",
    x = "Apparent Velocity (mm/hr)",
    y = "Limiting Dissolved Oxygen (ppm)",
    color = "Egg Quantity"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Display the plot
print(srf2_plot)
