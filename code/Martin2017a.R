# Martin, B. T., Pike, A., John, S. N., Hamda, N., Roberts, J., Lindley, S. T., & Danner, E. M. (2017). 
# Phenomenological vs. biophysical models of thermal stress in aquatic eggs. Ecology Letters, 20(1), 50–59
# Martin et al. 2017: Chinook embryo survival vs incubation temperature (Lab-parameterized)
# Equation 1-2

# Load libraries
library(ggplot2)

# 1. Define the Temperature range (x-axis)
T_seq <- seq(8, 22, by = 0.5)

# 2. Define shared biological parameters for Chinook development
# Relative development increases daily until it reaches 1 (Zueg et al. 2012)
dev_rate <- (0.001044 * T_seq) + 0.00056
n_days <- 1 / dev_rate # Total days in embryonic stage 

# 3. Lab-parameterized model parameters
T_crit_lab <- 15.4 # Temperature threshold for lab (°C)
b_T_lab <- 0.034   # Mortality rate increase above threshold
S_B_lab <- 80.0    # Background survival (%)

# Daily instantaneous mortality rate
h_lab <- b_T_lab * pmax(T_seq - T_crit_lab, 0)

# Total egg-to-fry survival (%)
survival_lab <- S_B_lab * exp(-n_days * h_lab)

# 4. Create DataFrame adhering to the e-Library formats
df_lab <- data.frame(
  curve.id = "Martin_2017_Lab",
  stressor.label = "incubation_temperature",
  stressor.x = T_seq,
  units.x = "degC",
  response.label = "egg_to_fry_survival",
  response.y = round(survival_lab, 2),
  units.y = "percent"
)

# 5. Export to CSV
write.csv(df_lab, "Martin_2017_Lab_SRF.csv", row.names = FALSE)
cat("Data successfully written to 'Martin_2017_Lab_SRF.csv'\n")

# 6. Plot the Curve
plot(df_lab$stressor.x, df_lab$response.y, 
     type = "l", col = "#1f77b4", lwd = 3,
     ylim = c(0, 100), 
     xlab = "Constant Incubation Temperature (°C)", 
     ylab = "Egg-to-Fry Survival (%)",
     main = "Lab-Parameterized Embryo Thermal Tolerance\n(Martin et al. 2017)",
     panel.first = grid())
