# -------------------------------------------------------------------------
# Simulation of Figure 4: Resting Metabolic Rate vs. Temperature
# Source: Dressler et al., 2023
# Code generated with Gemini 3.1 Pro on 6/30/26 using a custom GEM 'SRF Data Extraction' by AH Fullerton
# -------------------------------------------------------------------------

# 1. Define the range of the stressor (Temperature in °C)
# Based on the x-axis limits of Figure 4 (14 to 28 °C)
temperatures <- seq(14, 28, by = 0.5)

# 2. Apply the equations provided in the figure panels
# Panel A: Piru Creek
rmr_piru <- 1.263 * exp(0.046 * temperatures)

# Panel B: Arroyo Seco
rmr_arroyo <- 0.130 * exp(0.126 * temperatures)

# 3. Create formatted data frames based on the Extracted Data schema
df_piru <- data.frame(
  curve.id = "Piru Creek",
  stressor.label = "Temperature",
  stressor.x = temperatures,
  units.x = "degC",
  response.label = "RMR",
  response.y = round(rmr_piru, 3),
  units.y = "mgO2 kg-1 min-1",
  plot.type = "curve"
)

df_arroyo <- data.frame(
  curve.id = "Arroyo Seco",
  stressor.label = "Temperature",
  stressor.x = temperatures,
  units.x = "degC",
  response.label = "RMR",
  response.y = round(rmr_arroyo, 3),
  units.y = "mgO2 kg-1 min-1",
  plot.type = "curve"
)

# Combine the two curves into a single dataset
df_combined <- rbind(df_piru, df_arroyo)

# 4. Export the data to a CSV file
write.csv(df_combined, "data/Dressler2023_Fig4_RMR_Simulation.csv", row.names = FALSE)

# 5. Reproduce the Figure
# Set up plotting parameters to match the paper's general aesthetic
plot(temperatures, rmr_piru, type = "l", col = "darkorange", lwd = 2,
     xlab = "Temperature (°C)", 
     ylab = expression(paste("RMR (mgO"[2], " kg"^{-1}, " min"^{-1}, ")")),
     ylim = c(0, max(c(rmr_piru, rmr_arroyo))),
     main = "Simulated RMR vs Temperature (Dressler 2023, Fig. 4)",
     las = 1)

# Add the Arroyo Seco curve
lines(temperatures, rmr_arroyo, col = "darkcyan", lwd = 2, lty = 2)

# Add a legend
legend("topleft", legend = c("Piru Creek (Panel A)", "Arroyo Seco (Panel B)"),
       col = c("darkorange", "darkcyan"), lty = c(1, 2), lwd = 2, bty = "n")
