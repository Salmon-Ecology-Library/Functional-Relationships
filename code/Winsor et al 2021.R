# Winsor S, Blumenshine S, Adelizi P, Bigelow M. Precocious Maturation in Spring-Run Chinook Salmon Is Affected by 
# Incubation Temperature, Feeding Regime, and Parentage. Transactions of the American Fisheries Society 150:578-592, 2021.
# https://doi.org/10.1002/tafs.10309

# 

# ---------------------------------------------------------
# Define the theoretical curve based on Mean ATU
# ---------------------------------------------------------
# Mean ATU calculated from the 10 lots in Table 1 (5 warm, 5 cold)
mean_atu <- 873.5

# Define a sequence of temperatures for the continuous curve (e.g., 7C to 14C)
temp_seq <- seq(7, 14, length.out = 100)

# Calculate predicted days to ponding based on the ATU formula
predicted_days <- mean_atu / temp_seq

# ---------------------------------------------------------
# Raw Data from Table 1 for plotting overlay
# ---------------------------------------------------------
raw_temp <- c(12.40, 12.40, 12.39, 12.47, 12.03,  # Warm lots 1-5
              9.23, 9.23, 9.27, 9.33, 9.38)       # Cold lots 1-5

raw_days <- c(63, 68, 66, 69, 73,                 # Warm lots 1-5
              94, 97, 95, 95, 90)                 # Cold lots 1-5

# ---------------------------------------------------------
# Plotting
# ---------------------------------------------------------
par(mfrow = c(1, 1), mar = c(5, 5, 4, 2))

# Plot the continuous theoretical curve
plot(temp_seq, predicted_days, type = "l", col = "blue", lwd = 2,
     ylim = c(50, 130), xlim = c(7, 14), las = 1,
     xlab = "Mean Incubation Temperature (C)", 
     ylab = "Days to Ponding", 
     main = "Days to Ponding vs. Incubation Temperature")

# Overlay the raw data points from the experiment
points(raw_temp, raw_days, pch = 16, col = "black", cex = 1.2)

# Add a legend
legend("topright", legend = c("Predicted Curve (ATU = 873.5)", "Observed Data (Table 1)"),
       col = c("blue", "black"), lty = c(1, NA), pch = c(NA, 16), lwd = c(2, NA), bty = "n")

# ---------------------------------------------------------
# Data Export
# ---------------------------------------------------------
# Export the curve data to a CSV for external use
dat_ponding_curve <- data.frame(Temperature_C = temp_seq, Predicted_Days = predicted_days)
write.csv(dat_ponding_curve, "Winsor_2021_DaysToPonding_Curve.csv", row.names = FALSE)
