# Lisle, T. E., & Lewis, J. (1992). Effects of sediment transport on survival of salmonid embryos in a natural 
# stream: a simulation approach. Canadian Journal of Fisheries and Aquatic Sciences, 49(11), 2337–2344.
# Lisle & Lewis 1992: Salmonid embryo survival vs fine bedload flux (Tappel & Bjornn approach)
# Equation 9C

# Load necessary libraries
library(dplyr)
library(ggplot2)

# 1. Define the stressor range (0 to 25,000 kg/m to capture 0% survival)
stressor_x <- seq(0, 25000, length.out = 20)

# 2. Calculate the biological response using the Lisle & Lewis 1992 equation
# Formula: S = 67.9 - 1.10 * (qb)^0.412
response_y <- pmax(0, 67.9 - 1.10 * (stressor_x^0.412))

# 3. Create the data frame
srf2_data <- data.frame(
  curve.id = rep("c2", length(stressor_x)),
  stressor.label = rep("fine_bedload_flux", length(stressor_x)),
  stressor.x = round(stressor_x, 2),
  units.x = rep("kg/m", length(stressor_x)),
  response.label = rep("expected_survival", length(stressor_x)),
  response.y = round(response_y, 2),
  units.y = rep("percent", length(stressor_x))
)

# 4. Plot the curve
plot(srf2_data$stressor.x, srf2_data$response.y, type = "l", col = "red", lwd = 2,
     xlab = "Fine Bedload Flux (kg/m)", 
     ylab = "Expected Survival (%)",
     main = "Survival vs. Bedload Flux (Tappel & Bjornn)")

# 5. Export to CSV
write.csv(srf2_data, "SRF2_Lisle_Lewis_1992_Data.csv", row.names = FALSE)
