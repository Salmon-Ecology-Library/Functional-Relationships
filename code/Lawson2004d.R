# Lawson, P. W., Logerwell, E. A., Mantua, N. J., Francis, R. C., & Agostini, V. N. (2004). 
# Environmental factors influencing freshwater survival and smolt production in Pacific Northwest coho salmon (Oncorhynchus kisutch). 
# Canadian Journal of Fisheries and Aquatic Sciences, 61(3), 360–373.
# Lawson et al. 2004: Queets Coho smolt production vs. annual air temp
# Table 6, Figure 6a

# Load necessary library
library(ggplot2)

# 1. Define ONLY the Annual Air Temperature coefficient from Lawson et al. 2004 (Table 6)
# We ignore the intercept and winter flow to match the partial regression plot
coeff_temp <- -54.8

# 2. Generate the stressor values (Annual Air Temperature, z-score)
# Fig 6a shows the data bounds roughly between -1.5 and 2.5
stressor_x <- seq(-1.5, 2.5, by = 0.25)

# Calculate the raw partial effect 
# (For a purely linear GAM term without an intercept, it naturally centers at 0,0)
response_y <- coeff_temp * stressor_x

# 3. Format dataframe
srf7_partial_data <- data.frame(
  curve.id = rep("Lawson2004_7_Partial", length(stressor_x)),
  stressor.label = rep("Annual_Air_Temperature", length(stressor_x)),
  stressor.x = stressor_x,
  units.x = rep("z-score", length(stressor_x)),
  response.label = rep("Partial_Effect_on_Smolts", length(stressor_x)),
  response.y = round(response_y, 2),
  units.y = rep("partial_contribution", length(stressor_x))
)

# Print the formatted data to the console
print(srf7_partial_data)

# Export directly to CSV
write.csv(srf7_partial_data, "Lawson2004_SRF7_Partial.csv", row.names = FALSE)

# 4. Generate a Plot matching Figure 6a
srf7_partial_plot <- ggplot(srf7_partial_data, aes(x = stressor.x, y = response.y)) +
  geom_line(color = "black", size = 1) +
  geom_point(shape = 1, size = 3) +
  labs(
    title = "Partial Regression for Annual Air Temp (Lawson et al. 2004 Fig 6a)",
    x = "AnnTemp (Standardized)",
    y = "Partial for AnnTemp (x 1000)"
  ) +
  scale_y_continuous(limits = c(-150, 150), breaks = seq(-150, 150, by = 50)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(srf7_partial_plot)
