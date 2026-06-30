
# -------------------------------------------------------------------------
# Simulation of Figure 6 (Panels A & C): Metabolic Rates and FAS
# Source: Dressler et al., 2023 (incorporating exact data from Table 1)
# Code generated with Gemini 3.1 Pro on 6/30/26 using a custom GEM 'SRF Data Extraction' by AH Fullerton
# -------------------------------------------------------------------------

# 1. Define the Tabular Data (Points from Table 1)
# -------------------------------------------------------------------------
# Piru Creek Data
piru_temp <- c(20, 23, 25)
piru_mmr <- c(10.19, 12.71, 14.34)
piru_rmr <- c(2.69, 3.38, 3.70)
piru_fas <- c(3.78, 4.05, 3.97)

# Arroyo Seco Data
arroyo_temp <- c(18, 19, 21)
arroyo_mmr <- c(7.43, 10.25, 10.64)
arroyo_rmr <- c(0.83, 1.72, 2.03)
arroyo_fas <- c(10.48, 6.76, 5.77)

# 2. Simulate the Continuous Curve for Arroyo Seco FAS (Panel C)
# -------------------------------------------------------------------------
# Equation: y = 34.5 - 1.4x (extrapolated to 24 C per the figure)
curve_temp <- seq(18, 24, by = 0.5)
arroyo_fas_curve <- 34.5 - (1.4 * curve_temp)

# 3. Format Data into the Requested Schema
# -------------------------------------------------------------------------
create_df <- function(id, x, y, resp_label, plot_type) {
  data.frame(
    curve.id = id,
    stressor.label = "MMR Temperature",
    stressor.x = x,
    units.x = "degC",
    response.label = resp_label,
    response.y = y,
    units.y = ifelse(resp_label == "FAS", "ratio", "mgO2 kg-1 min-1"),
    plot.type = plot_type
  )
}

df_points <- rbind(
  create_df("Piru_MMR", piru_temp, piru_mmr, "MMR", "scatter"),
  create_df("Piru_RMR", piru_temp, piru_rmr, "RMR", "scatter"),
  create_df("Piru_FAS", piru_temp, piru_fas, "FAS", "scatter"),
  create_df("Arroyo_MMR", arroyo_temp, arroyo_mmr, "MMR", "scatter"),
  create_df("Arroyo_RMR", arroyo_temp, arroyo_rmr, "RMR", "scatter"),
  create_df("Arroyo_FAS", arroyo_temp, arroyo_fas, "FAS", "scatter")
)

df_curve <- create_df("Arroyo_FAS_Model", curve_temp, arroyo_fas_curve, "FAS", "curve")

# Combine all data and export
df_combined <- rbind(df_points, df_curve)
write.csv(df_combined, "data/Dressler2023_Fig6_A_C.csv", row.names = FALSE)

# 4. Reproduce the Figure (Stacked Panels A and C)
# -------------------------------------------------------------------------
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))

# Panel A: MMR and RMR
plot(NULL, xlim = c(17.5, 25.5), ylim = c(0, 16),
     xlab = "MMR Temperature (°C)", 
     ylab = expression(paste("Metabolic Rate (mgO"[2], " kg"^{-1}, " min"^{-1}, ")")),
     main = "Panel A: MMR and RMR", las = 1)

# Plot Points (Triangles for MMR, Squares for RMR)
points(piru_temp, piru_mmr, pch = 17, col = "darkorange", cex = 1.5)
points(piru_temp, piru_rmr, pch = 15, col = "darkorange", cex = 1.5)
points(arroyo_temp, arroyo_mmr, pch = 17, col = "darkcyan", cex = 1.5)
points(arroyo_temp, arroyo_rmr, pch = 15, col = "darkcyan", cex = 1.5)

legend("bottomright", legend = c("Piru MMR", "Piru RMR", "Arroyo MMR", "Arroyo RMR"),
       col = c("darkorange", "darkorange", "darkcyan", "darkcyan"), 
       pch = c(17, 15, 17, 15), bty = "n")

# Panel C: FAS
plot(NULL, xlim = c(17.5, 25.5), ylim = c(0, 12),
     xlab = "MMR Temperature (°C)", 
     ylab = "FAS",
     main = "Panel C: Factorial Aerobic Scope (FAS)", las = 1)

# Plot Points (Circles)
points(piru_temp, piru_fas, pch = 16, col = "darkorange", cex = 1.5)
points(arroyo_temp, arroyo_fas, pch = 16, col = "darkcyan", cex = 1.5)

# Plot Simulated Line for Arroyo Seco
lines(curve_temp, arroyo_fas_curve, col = "darkcyan", lwd = 2, lty = 2)

legend("topright", legend = c("Piru FAS", "Arroyo FAS", "y = 34.5 - 1.4x"),
       col = c("darkorange", "darkcyan", "darkcyan"), 
       pch = c(16, 16, NA), lty = c(NA, NA, 2), lwd = c(NA, NA, 2), bty = "n")

# Reset plotting layout
par(mfrow = c(1, 1))
