# Bellmore, J. R., Sergeant, C. J., Bellmore, R. A., Falke, J. A., & Fellman, J. B. (2022). Modeling coho salmon (Oncorhynchus kisutch) population response to 
# streamflow and water temperature extremes. Canadian Journal of Fisheries and Aquatic Sciences, 80(2), 243–260.
# Bellmore et al. 2022: Stream bed Shields stress vs egg scour mortality
# Equation 7

# 1. Define the parameters
d_egg <- 35.0      # Egg burial depth (cm)
epsilon_base <- 3.33     # Scour strength coefficient baseline

# Define uncertainty bounds for epsilon
# A lower epsilon creates a smaller negative exponent, increasing mortality (Upper Limit)
# A higher epsilon creates a larger negative exponent, decreasing mortality (Lower Limit)
epsilon_low <- 3.00   # Triggers Upper Limit
epsilon_high <- 3.66  # Triggers Lower Limit

slope <- 0.022     # Channel slope (m/m)

# 2. Calculate Critical Shields Stress (tau_c_star)
tau_c_star <- 0.15 * (slope^0.25)

# 3. Generate a sequence of dimensionless Shields stress (tau_star)
tau_star_seq <- seq(0.00, 0.20, by = 0.01)

# 4. Calculate Egg Scour Mortality 
mortality_base <- exp(-d_egg * (epsilon_base * exp(-1.52 * (tau_star_seq / tau_c_star))))
mortality_upper <- exp(-d_egg * (epsilon_low * exp(-1.52 * (tau_star_seq / tau_c_star))))
mortality_lower <- exp(-d_egg * (epsilon_high * exp(-1.52 * (tau_star_seq / tau_c_star))))

# 5. Create the dataframe
eggscour_df <- data.frame(
  curve.id = "c3",
  stressor.label = "shields_stress",
  stressor.x = tau_star_seq,
  units.x = "dimensionless",
  response.label = "egg_mortality",
  response.y = round(mortality_base, 4), 
  units.y = "proportion",
  lower.limit = round(mortality_lower, 4),
  upper.limit = round(mortality_upper, 4)
)

# 6. Export the data to a CSV file
write.csv(eggscour_df, "ShieldsStress_EggMortality.csv", row.names = FALSE)

# 7. plot the curve
plot(eggscour_df$stressor.x, eggscour_df$response.y, 
     type = "l", 
     col = "blue", 
     lwd = 2,
     xlab = "Shields Stress (t*)", 
     ylab = "Egg Scour Mortality (Proportion)",
     main = "Modeled Egg Scour Mortality vs. Shields Stress")
