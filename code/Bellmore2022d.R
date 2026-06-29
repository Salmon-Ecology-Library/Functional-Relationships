# Bellmore, J. R., Sergeant, C. J., Bellmore, R. A., Falke, J. A., & Fellman, J. B. (2022). Modeling coho salmon (Oncorhynchus kisutch) population response to 
# streamflow and water temperature extremes. Canadian Journal of Fisheries and Aquatic Sciences, 80(2), 243–260.
# Based on Equation 12 and Table S1 parameters

# Load required libraries
library(ggplot2)

# 1. Define the parameters
beta_starve <- 10       
HS_starve <- 0.6   

# Define baseline and uncertainty bounds for alpha_starve
alpha_base <- 1.0     
alpha_low <- 0.75     # Lower strength = lower mortality (Lower Limit)
alpha_high <- 1.25    # Higher strength = higher mortality (Upper Limit)

# 2. Generate a sequence of Condition Factors (CF)
cf_seq <- seq(0.40, 1.10, by = 0.05)

# 3. Calculate Starvation Mortality (Base, Lower Limit, Upper Limit)
# Formula: Starve_mort = alpha * (1 - (CF^beta) / (CF^beta + HS^beta))
mortality_base <- alpha_base * (1 - (cf_seq^beta_starve) / ((cf_seq^beta_starve) + (HS_starve^beta_starve)))
mortality_lower <- alpha_low * (1 - (cf_seq^beta_starve) / ((cf_seq^beta_starve) + (HS_starve^beta_starve)))

# Use pmin() to cap the upper limit at 1.0 (100% mortality)
mortality_upper <- pmin(1.0, alpha_high * (1 - (cf_seq^beta_starve) / ((cf_seq^beta_starve) + (HS_starve^beta_starve))))

# 4. Create the dataframe
juvmort_df <- data.frame(
  curve.id = "c1",
  stressor.label = "juvenile_condition_factor",
  stressor.x = cf_seq,
  units.x = "dimensionless",
  response.label = "starvation_mortality",
  response.y = round(mortality_base, 4), 
  units.y = "proportion",
  lower.limit = round(mortality_lower, 4),
  upper.limit = round(mortality_upper, 4)
)

# 5. Export the data to a CSV file
write.csv(juvmort_df, "CF_StarvationMortality.csv", row.names = FALSE)

# 6. plot the curve
plot(juvmort_df$stressor.x, juvmort_df$response.y, 
     type = "l", 
     col = "blue", 
     lwd = 2,
     xlab = "Juvenile Condition Factor", 
     ylab = "Daily Juvenile Mortality",
     main = "Modeled Daily Juvenile Mortality vs. Juvenile Condition Factor")
