# Martin, B. T., Pike, A., John, S. N., Hamda, N., Roberts, J., Lindley, S. T., & Danner, E. M. (2017). 
# Phenomenological vs. biophysical models of thermal stress in aquatic eggs. Ecology Letters, 20(1), 50–59
# Martin et al. 2017: Chinook embryo survival vs in-river temperature (Field-parameterized)
# Equation 1-2

# 1. Define the Temperature range (x-axis)
T_seq <- seq(8, 22, by = 0.5)

# 2. Field-parameterized model parameters (from text)
T_crit_field <- 12.0 # Field temperature threshold (°C)
b_T_field <- 0.024   # Field mortality rate increase above threshold
S_B_field <- 28.0    # Background survival without density-dependence (%)

# 3. Calculate Development Time and Mortality
# Maturation rate (Zueg et al. 2012)
dev_rate <- (0.001044 * T_seq) + 0.00056
n_days <- 1 / dev_rate 

# Daily instantaneous mortality rate
h_field <- b_T_field * pmax(T_seq - T_crit_field, 0)

# Total egg-to-fry survival (%)
survival_field <- S_B_field * exp(-n_days * h_field)

# 4. Create DataFrame adhering to the e-Library formats
df_field <- data.frame(
  curve.id = "Martin_2017_Field_Survival",
  stressor.label = "temperature",
  stressor.x = T_seq,
  units.x = "degC",
  response.label = "egg_to_fry_survival",
  response.y = round(survival_field, 2),
  units.y = "percent"
)

# 5. Export to CSV
write.csv(df_field, "Martin_2017_Field_Survival_SRF.csv", row.names = FALSE, quote = FALSE)
cat("Data successfully written to 'Martin_2017_Field_Survival_SRF.csv'\n")

# 6. Plot the Curve
plot(df_field$stressor.x, df_field$response.y, 
     type = "l", col = "#ff7f0e", lwd = 3,
     ylim = c(0, 35),
     xlab = "Constant Incubation Temperature (°C)", 
     ylab = "Egg-to-Fry Survival (%)",
     main = "Field-Parameterized Embryo Thermal Tolerance\n(Martin et al. 2017)",
     panel.first = grid())
