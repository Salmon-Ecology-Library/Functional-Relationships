# Lawson, P. W., Logerwell, E. A., Mantua, N. J., Francis, R. C., & Agostini, V. N. (2004). 
# Environmental factors influencing freshwater survival and smolt production in Pacific Northwest coho salmon (Oncorhynchus kisutch). 
# Canadian Journal of Fisheries and Aquatic Sciences, 61(3), 360–373.
# Lawson et al. 2004: Coho smolt production vs. 2nd winter flow
# Table 2, Figure 3c

# Load necessary libraries
library(ggplot2)
library(splines)

# 1. Define model coefficients for the 2nd Winter Flow b-spline (Table 2)
# We ignore the intercept and other variables to match the partial regression plot
coeff_winter1 <- 2444.5 # Left leg of the b-spline (x <= 0)
coeff_winter2 <- 1996.9 # Right leg of the b-spline (x > 0)

# 2. Define the stressor values (2nd Winter Flow, z-score)
# Fig 3c shows the data bounds roughly between -2.5 and 2.5
stressor_x <- seq(-2.5, 2.5, by = 0.25)

# 3. Generate the 1-degree b-spline basis mathematically identically to the paper
# The authors set the knot at 0. Boundary knots define where the basis drops to 0.
b_spline <- bs(stressor_x, degree = 1, knots = 0, Boundary.knots = c(-2.5, 2.5))

# 4. Calculate the raw partial effect
raw_partial <- (coeff_winter1 * b_spline[, 1]) + (coeff_winter2 * b_spline[, 2])

# In GAM partial plots (like Fig 3c), the curve is mean-centered around 0.
# We center the curve by subtracting its mean.
mean_centered_partial <- raw_partial - mean(raw_partial)

# 5. Format dataframe
srf3_partial_data <- data.frame(
  curve.id = rep("Lawson2004_3_Partial", length(stressor_x)),
  stressor.label = rep("2nd_Winter_Flow", length(stressor_x)),
  stressor.x = stressor_x,
  units.x = rep("z-score", length(stressor_x)),
  response.label = rep("Partial_Effect_on_Smolts", length(stressor_x)),
  response.y = round(mean_centered_partial, 2),
  units.y = rep("partial_contribution", length(stressor_x))
)

# Print the formatted data to the console
print(srf3_partial_data)

# Optional: Export directly to CSV
write.csv(srf3_partial_data, "Lawson2004_SRF3.csv", row.names = FALSE)

# 6. Generate a Plot matching Figure 3c
partial_plot <- ggplot(srf3_partial_data, aes(x = stressor.x, y = response.y)) +
  geom_line(color = "black", size = 1) +
  geom_point(shape = 1, size = 3) + 
  labs(
    title = "Partial Regression for 2nd Winter Flow (Lawson et al. 2004 Fig 3c)",
    x = "Flow.NDJFM.t1 (Standardized)",
    y = "bs(Flow.NDJFM.t1, knots = 0)"
  ) +
  scale_y_continuous(limits = c(-2000, 1500), breaks = seq(-2000, 1000, by = 1000)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(partial_plot)
