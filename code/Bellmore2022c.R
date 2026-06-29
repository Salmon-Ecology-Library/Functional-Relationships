# Bellmore, J. R., Sergeant, C. J., Bellmore, R. A., Falke, J. A., & Fellman, J. B. (2022). Modeling coho salmon (Oncorhynchus kisutch) population response to streamflow 
# and water temperature extremes. Canadian Journal of Fisheries and Aquatic Sciences, 80(2), 243–260.
# Based on Equation 1 and Table S1 parameters

# 1. Define the parameters
beta_spawner <- 16       # Shape parameter

# Define baseline and uncertainty bounds for HS_spawner
HS_base <- 0.70          
HS_low <- 0.525          # Lower HS means fish survive at worse conditions (Lower Limit Mortality)
HS_high <- 0.875         # Higher HS means fish die at better conditions (Upper Limit Mortality)

# 2. Generate a sequence of Condition Factors (CF)
cf_seq <- seq(0.40, 1.00, by = 0.05)

# 3. Calculate Pre-spawn Mortality (Base, Lower Limit, Upper Limit)
# Formula: Prespawn Mort = 1 - (CF^beta) / (CF^beta + HS^beta)
mortality_base <- 1 - (cf_seq^beta_spawner) / ((cf_seq^beta_spawner) + (HS_base^beta_spawner))
mortality_lower <- 1 - (cf_seq^beta_spawner) / ((cf_seq^beta_spawner) + (HS_low^beta_spawner))
mortality_upper <- 1 - (cf_seq^beta_spawner) / ((cf_seq^beta_spawner) + (HS_high^beta_spawner))

# 4. Create the dataframe
CFmort_df <- data.frame(
  curve.id = "c4",
  stressor.label = "spawner_condition_factor",
  stressor.x = cf_seq,
  units.x = "dimensionless",
  response.label = "prespawn_mortality",
  response.y = round(mortality_base, 4), 
  units.y = "proportion",
  stressor.value = "Equation_1",
  lower.limit = round(mortality_lower, 4),
  upper.limit = round(mortality_upper, 4)
)

# 5. Export the data to a CSV file
write.csv(CFmort_df, "CF_PrespawnMortality.csv", row.names = FALSE)

# 7. plot the curve
plot(CFmort_df$stressor.x, CFmort_df$response.y, 
     type = "l", 
     col = "blue", 
     lwd = 2,
     xlab = "Spawner Condition Factor", 
     ylab = "Prespawn Mortality",
     main = "Modeled Prespawn Mortality vs. Spawner Condition Factor")
