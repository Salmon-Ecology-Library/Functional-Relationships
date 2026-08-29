# R script to generate and plot extracted data for Lundin et al. 2019

# Lundin, J. I., Spromberg, J. A., Jorgensen, J. C., Myers, J. M., Chittaro, P. M., Zabel, R. W., 
# Johnson, L. L., Neely, R. M., & Scholz, N. L. (2019). Legacy habitat contamination as a limiting 
# factor for Chinook salmon recovery in the Willamette Basin, Oregon, USA. PLoS ONE, 14(3), e0214399.
# Lundin et al. 2019: Chinook salmon estuary survival vs change in fish length (contaminant proxy)
# Equation 1


# Load libraries
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Define the stressor values (change in length in mm)
# Includes -1.04 mm and -7.02 mm as referenced in the paper for PCB and legacy contaminant exposure
delta_length <- c(-10.0, -7.02, -5.0, -1.04, 0.0, 5.0, 10.0)

# Define the survival function based on the adapted Zabel and Achord (2004) equation
# Intercept calibrated to -2.003 to reflect 0.1188 baseline survival for spring subyearlings
estuary_survival <- function(dl) {
  val <- exp(-2.003 + (0.0329 * dl))
  return(val / (1 + val))
}

# Calculate response values
survival_prob <- estuary_survival(delta_length)

# Create the dataframe
df <- data.frame(
  curve.id = rep("Lundin2019_c1", length(delta_length)),
  stressor.label = rep("change_in_length", length(delta_length)),
  stressor.x = delta_length,
  units.x = rep("mm", length(delta_length)),
  response.label = rep("estuary_survival", length(delta_length)),
  response.y = round(survival_prob, 6),
  units.y = rep("proportion", length(delta_length))
)

# Output the dataframe to a CSV file
write.csv(df, "Lundin2019_F1_Extracted_Data.csv", row.names = FALSE, quote = FALSE)

# Print the resulting dataframe to the console
print(df)


# --- 2. PLOT THE DATA ---

# Generate a sequence of values for a smooth curve
plot_x <- seq(-15, 15, length.out = 100)
plot_y <- estuary_survival(plot_x)
smooth_data <- data.frame(stressor.x = plot_x, response.y = plot_y)

# Create the plot
p <- ggplot() +
  geom_line(data = smooth_data, aes(x = stressor.x, y = response.y), color = "blue", linewidth = 1) +
  geom_point(data = df, aes(x = stressor.x, y = response.y), color = "red", size = 3) +
  
  # Annotate the specific points of interest from the paper
  annotate("text", x = -8.5, y = 0.11, label = "-7.02 mm\n(Legacy Toxics)", size = 3.5) +
  annotate("segment", x = -8.5, xend = -7.2, y = 0.105, yend = 0.098, arrow = arrow(length = unit(0.2, "cm"))) +
  
  annotate("text", x = -2.5, y = 0.13, label = "-1.04 mm\n(PCBs)", size = 3.5) +
  annotate("segment", x = -2.5, xend = -1.2, y = 0.125, yend = 0.117, arrow = arrow(length = unit(0.2, "cm"))) +
  
  # Formatting
  scale_y_continuous(limits = c(0.07, 0.18)) +
  labs(
    title = "Estuary Survival vs Change in Fish Length (Lundin et al. 2019)",
    x = "Change in Length (mm)",
    y = "Estuary Survival (Proportion)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank()
  )
# Display the plot
print(p)
