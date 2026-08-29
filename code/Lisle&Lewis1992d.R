# Lisle, T. E., & Lewis, J. (1992). Effects of sediment transport on survival of salmonid embryos in a natural 
# stream: a simulation approach. Canadian Journal of Fisheries and Aquatic Sciences, 49(11), 2337–2344.
# Lisle & Lewis 1992: Salmonid embryo survival vs fine bedload flux (Intergravel flow approach)
# Equation 10C

# Load necessary libraries
library(dplyr)
library(ggplot2)

# 1. Define the stressor range (0 to 4000 kg/m based on zero-intercept)
stressor_x <- seq(0, 4000, length.out = 20)

# 2. Calculate the biological response using the Lisle & Lewis 1992 equation
# Formula: S = 56.8 - 1.864 * (qb)^0.412
response_y <- pmax(0, 56.8 - 1.864 * (stressor_x^0.412))

# 3. Create the data frame
srf4_data <- data.frame(
  curve.id = rep("c4", length(stressor_x)),
  stressor.label = rep("fine_bedload_flux", length(stressor_x)),
  stressor.x = round(stressor_x, 2),
  units.x = rep("kg/m", length(stressor_x)),
  response.label = rep("expected_survival", length(stressor_x)),
  response.y = round(response_y, 2),
  units.y = rep("percent", length(stressor_x))
)

# 4. Plot the curve
plot(srf4_data$stressor.x, srf4_data$response.y, type = "l", col = "purple", lwd = 2,
     xlab = "Fine Bedload Flux (kg/m)", 
     ylab = "Expected Survival (%)",
     main = "Survival vs. Bedload Flux (Intergravel Flow)")

# 5. Export to CSV
write.csv(srf4_data, "SRF4_Lisle_Lewis_1992_Data.csv", row.names = FALSE)
