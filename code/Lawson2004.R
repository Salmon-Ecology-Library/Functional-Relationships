# Lawson, P. W., Logerwell, E. A., Mantua, N. J., Francis, R. C., & Agostini, V. N. (2004). 
# Environmental factors influencing freshwater survival and smolt production in Pacific Northwest coho salmon (Oncorhynchus kisutch). 
# Canadian Journal of Fisheries and Aquatic Sciences, 61(3), 360–373.
# Lawson et al. 2004: Coho smolt production vs. annual air temp
# Table 2, Figure 3a


# Load necessary library for plotting
library(ggplot2)

# 1. Define ONLY the temperature coefficient from Lawson et al. 2004 (Table 2)
# We are ignoring the intercept and other variables to match the partial regression plot
coeff_temp <- -910.24

# 2. Generate the stressor values (x-axis)
# Annual temperature is a standardized z-score. We will span from -1.5 to +1.25 to match Fig 3a.
stressor_x <- seq(-1.5, 1.25, by = 0.25)

# Calculate the partial regression effect (y-axis)
response_y <- (coeff_temp * stressor_x)

# 3. Create a data frame
srf1_partial_data <- data.frame(
  curve.id = rep("Lawson2004_1_Partial", length(stressor_x)),
  stressor.label = rep("Annual_Air_Temperature", length(stressor_x)),
  stressor.x = stressor_x,
  units.x = rep("z-score", length(stressor_x)),
  response.label = rep("Partial_Effect_on_Smolts", length(stressor_x)),
  response.y = round(response_y, 2),
  units.y = rep("partial_contribution", length(stressor_x))
)

# Print the formatted data to the console 
print("Partial effect data formatted for the template:")
print(srf1_partial_data)

# 4. Generate the plot matching Figure 3a
srf1_partial_plot <- ggplot(srf1_partial_data, aes(x = stressor.x, y = response.y)) +
  geom_line(color = "black", size = 1) +
  geom_point(shape = 1, size = 3) + # Open circles to match the paper's style
  labs(
    title = "Partial Regression for AnnTemp (Matches Lawson et al. 2004 Fig 3a)",
    x = "AnnTemp (Standardized)",
    y = "Partial for AnnTemp"
  ) +
  scale_y_continuous(limits = c(-2000, 2000), breaks = seq(-2000, 2000, by = 1000)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Display the plot
print(srf1_partial_plot)
