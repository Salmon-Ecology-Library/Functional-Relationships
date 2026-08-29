# Lawson, P. W., Logerwell, E. A., Mantua, N. J., Francis, R. C., & Agostini, V. N. (2004). 
# Environmental factors influencing freshwater survival and smolt production in Pacific Northwest coho salmon (Oncorhynchus kisutch). 
# Canadian Journal of Fisheries and Aquatic Sciences, 61(3), 360–373.
# Lawson et al. 2004: Queets Coho smolt production vs. 2nd Winter Flow
# Table 6, Figure 6b

# Load necessary libraries
library(ggplot2)
library(splines)

# 1. Define model coefficients for Queets 2nd Winter Flow b-spline (Table 6)
# Ignore intercept and Annual Temp to match the partial regression plot
coeff_winter1 <- 113.7  # Left leg of the b-spline (x <= 0)
coeff_winter2 <- -55.8  # Right leg of the b-spline (x > 0)

# 2. Generate the stressor values (2nd Winter Flow, z-score)
# Fig 6b shows the data bounds roughly between -2.5 and 2.5
stressor_x <- seq(-2.5, 2.5, by = 0.25)

# 3. Generate the 1-degree b-spline basis mathematically identically to the paper
b_spline <- bs(stressor_x, degree = 1, knots = 0, Boundary.knots = c(-2.5, 2.5))

# 4. Calculate the raw partial effect
raw_partial <- (coeff_winter1 * b_spline[, 1]) + (coeff_winter2 * b_spline[, 2])

# In GAM partial plots (like Fig 6b), the curve is mean-centered around 0.
mean_centered_partial <- raw_partial - mean(raw_partial)

# 5. Format dataframe
srf8_partial_data <- data.frame(
  curve.id = rep("Lawson2004_8_Partial", length(stressor_x)),
  stressor.label = rep("2nd_Winter_Flow", length(stressor_x)),
  stressor.x = stressor_x,
  units.x = rep("z-score", length(stressor_x)),
  response.label = rep("Partial_Effect_on_Smolts", length(stressor_x)),
  response.y = round(mean_centered_partial, 2),
  units.y = rep("partial_contribution", length(stressor_x))
)

# Print the formatted data to the console
print(srf8_partial_data)

# Export directly to CSV
write.csv(srf8_partial_data, "Lawson2004_SRF8_Partial.csv", row.names = FALSE)

# 6. Generate a Plot matching Figure 6b
srf8_partial_plot <- ggplot(srf8_partial_data, aes(x = stressor.x, y = response.y)) +
  geom_line(color = "black", size = 1) +
  geom_point(shape = 1, size = 3) +
  labs(
    title = "Partial Regression for 2nd Winter Flow (Lawson et al. 2004 Fig 6b)",
    x = "Flow.NDJFM.t1 (Standardized)",
    y = "bs(Flow.NDJFM.t1, knots = 0) (x 1000)"
  ) +
  scale_y_continuous(limits = c(-200, 150), breaks = seq(-200, 150, by = 100)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(srf8_partial_plot)
