# Lawson, P. W., Logerwell, E. A., Mantua, N. J., Francis, R. C., & Agostini, V. N. (2004). 
# Environmental factors influencing freshwater survival and smolt production in Pacific Northwest coho salmon (Oncorhynchus kisutch). 
# Canadian Journal of Fisheries and Aquatic Sciences, 61(3), 360–373.
# Lawson et al. 2004: Coho smolt production vs. 2nd winter flow
# Table 2, Figure 3d

# Load necessary library
library(ggplot2)

# 1. Define ONLY the 2nd Spring Flow coefficient from Lawson et al. 2004 (Table 2)
# We ignore the intercept and other variables to match the partial regression plot
coeff_spring <- 558.58

# 2. Generate the stressor values (2nd Spring Flow, z-score)
# Fig 3d shows the data bounds roughly between -1.5 and 2.0
stressor_x <- seq(-1.5, 2.0, by = 0.25)

# Calculate the raw partial effect 
# (For a purely linear GAM term without an intercept, it naturally centers at 0,0)
response_y <- coeff_spring * stressor_x

# 3. Format dataframe
srf4_partial_data <- data.frame(
  curve.id = rep("Lawson2004_4_Partial", length(stressor_x)),
  stressor.label = rep("2nd_Spring_Flow", length(stressor_x)),
  stressor.x = stressor_x,
  units.x = rep("z-score", length(stressor_x)),
  response.label = rep("Partial_Effect_on_Smolts", length(stressor_x)),
  response.y = round(response_y, 2),
  units.y = rep("partial_contribution", length(stressor_x))
)

# Print the formatted data to the console
print(srf4_partial_data)

# Export directly to CSV
write.csv(srf4_partial_data, "Lawson2004_SRF4_Partial.csv", row.names = FALSE)

# 4. Generate a Plot matching Figure 3d
srf4_partial_plot <- ggplot(srf4_partial_data, aes(x = stressor.x, y = response.y)) +
  geom_line(color = "black", size = 1) +
  geom_point(shape = 1, size = 3) + 
  labs(
    title = "Partial Regression for 2nd Spring Flow (Lawson et al. 2004 Fig 3d)",
    x = "Flow.AMJ.t1 (Standardized)",
    y = "Partial for Flow.AMJ.t1"
  ) +
  scale_y_continuous(limits = c(-2000, 2000), breaks = seq(-2000, 2000, by = 1000)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(srf4_partial_plot)
