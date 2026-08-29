# Lisle, T. E., & Lewis, J. (1992). Effects of sediment transport on survival of salmonid embryos in a natural 
# stream: a simulation approach. Canadian Journal of Fisheries and Aquatic Sciences, 49(11), 2337–2344.
# Lisle & Lewis 1992: Salmonid embryo survival vs fine-sediment infiltration (Tappel & Bjornn approach)
# Equation 9B

# Load necessary libraries
library(dplyr)
library(ggplot2)

# 1. Define the stressor range (0 to 105 kg/m2 based on zero-intercept)
stressor_x <- seq(0, 105, length.out = 20)

# 2. Calculate the biological response using the Lisle & Lewis 1992 equation
# Formula: S = 67.9 - 0.648 * I
# We use pmax(0, ...) to ensure survival doesn't drop below 0%
response_y <- pmax(0, 67.9 - 0.648 * stressor_x)

# 3. Create the data frame
srf1_data <- data.frame(
  curve.id = rep("c1", length(stressor_x)),
  stressor.label = rep("fine_sediment_infiltration", length(stressor_x)),
  stressor.x = round(stressor_x, 2),
  units.x = rep("kg/m2", length(stressor_x)),
  response.label = rep("expected_survival", length(stressor_x)),
  response.y = round(response_y, 2),
  units.y = rep("percent", length(stressor_x))
)

# 4. Plot the curve
plot(srf1_data$stressor.x, srf1_data$response.y, type = "l", col = "blue", lwd = 2,
     xlab = "Fine-Sediment Infiltration (kg/m2)", 
     ylab = "Expected Survival (%)",
     main = "Survival vs. Infiltration (Tappel & Bjornn)")

# 5. Export to CSV
write.csv(srf1_data, "SRF1_Lisle_Lewis_1992_Data.csv", row.names = FALSE)
