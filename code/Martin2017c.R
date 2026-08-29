# Martin, B. T., Pike, A., John, S. N., Hamda, N., Roberts, J., Lindley, S. T., & Danner, E. M. (2017). 
# Phenomenological vs. biophysical models of thermal stress in aquatic eggs. Ecology Letters, 20(1), 50–59
# Martin et al. 2017: Oxygen limitation temperature vs flow velocity
# Equation 10 - Appendix S1

# Load libraries
library(ggplot2)

# 1. Define Biological Parameters
M <- 0.05               # Tissue mass immediately before hatching (g)
b0 <- 81.8 / 3600       # Convert oxygen demand scalar to (ug O2 / g / s)
b1 <- 0.0945            # Temperature scalar for demand (1/degC)
k_e <- 3.65e-4          # Mass transfer coefficient of the embryo (ug O2 / cm^2 s kPa)
P_i_star <- 6.1         # Critical internal oxygen tension (kPa)
R_egg <- 0.40           # Egg radius (cm)

# 2. Define Ambient Oxygen Parameter (100% Saturation)
P_e <- 101.325 * 0.20946 # ~21.22 kPa

# 3. Create a sequence of developmental temperatures to evaluate (deg C)
T_seq <- seq(5, 17.5, by = 0.1)

# Vector to store results
U_crit <- numeric(length(T_seq))

# 4. Loop through temperatures to calculate dynamic physics and U_crit
for(i in 1:length(T_seq)) {
  T_C <- T_seq[i]
  T_K <- T_C + 273.15 # Convert to Kelvin
  
  # --- A.1 Conversion factor (delta) ---
  A <- -135.90205; B <- 1.58E+05; C_param <- -6.64E+07
  D_param <- 1.24E+10; E <- -8.62E+11
  
  exponent <- A + (B/T_K) + (C_param/(T_K^2)) + (D_param/(T_K^3)) + (E/(T_K^4))
  C_sat <- 0.0319988 * exp(exponent)     # Saturation concentration (ug O2 / cm3)
  delta <- P_e / C_sat                   # Conversion factor (kPa / [ug O2 / cm3])
  
  # --- A.2 Kinematic viscosity (v) ---
  mu_kg_ms <- 2.414e-5 * 10^(247.8 / (T_K - 140)) # Dynamic viscosity (kg / m s)
  v <- (mu_kg_ms / 1000) * 10000                  # Kinematic viscosity (cm2 / s)
  
  # --- A.3 Diffusion coefficient of oxygen in water (D) ---
  D <- (7.4e-8 * sqrt(2.26 * 18) * T_K) / (mu_kg_ms * 1000 * (25.6^0.6)) # cm2 / s
  
  # --- Biological Calculations ---
  # 1. Oxygen Demand
  N <- b0 * M * exp(b1 * T_C) # Oxygen Demand (ug O2 / s)
  
  # Max possible supply at infinite flow (4 * pi * R^2 * k_e * (P_e - P_i_star))
  max_supply <- 4 * pi * R_egg^2 * k_e * (P_e - P_i_star)
  
  # If demand exceeds maximum theoretical supply, the fish cannot survive
  if(N >= max_supply) {
    U_crit[i] <- NA 
  } else {
    
    # 4. Critical Flow Velocity (Verbatim Equation 10)
    # U_crit = { v * [ (5 * N * delta * R * k_e) / (2 * D * (N - max_supply)) + (5/2) ]^2 } / { 2 * R * (v / D)^(2/3) }
    
    term1_numerator <- 5 * N * delta * R_egg * k_e
    term1_denominator <- 2 * D * (N - max_supply)
    
    bracket_term <- (term1_numerator / term1_denominator) + (5 / 2)
    
    numerator <- v * (bracket_term)^2
    denominator <- 2 * R_egg * (v / D)^(2/3)
    
    U_crit[i] <- numerator / denominator
  }
}

# 5. Filter valid data points 
valid_indices <- which(!is.na(U_crit) & U_crit > 0)
U_crit_valid <- U_crit[valid_indices]
T_valid <- T_seq[valid_indices]

# 6. Create DataFrame adhering to the e-Library format
df_biophysical <- data.frame(
  curve.id = "Martin_2017_Biophysical_Eq10",
  stressor.label = "flow_velocity",
  stressor.x = signif(U_crit_valid, 4),
  units.x = "cm_s",
  response.label = "oxygen_limitation_temperature",
  response.y = round(T_valid, 2),
  units.y = "degC"
)

# 7. Export the Data to CSV
write.csv(df_biophysical, "Martin_2017_Biophysical_Eq10_SRF.csv", row.names = FALSE)
cat("Data successfully written to 'Martin_2017_Biophysical_Eq10_SRF.csv'\n")

# 8. Plot the Curve
plot(df_biophysical$stressor.x, df_biophysical$response.y, 
     type = "l", col = "black", lwd = 3,
     xlab = expression(paste("Flow Velocity (cm ", s^-1, ")")), 
     ylab = "Oxygen Limitation Temperature (°C)",
     main = "Mechanistic Thermal Tolerance (Martin et al. 2017) Equation 10",
     panel.first = grid())
# Add approximate shaded regions for Lab and Field flows (optional)
rect(0.1, 15, 0.18, 15.8, col = rgb(0.12, 0.47, 0.71, 0.5), border = NA)
text(0.14, 14.5, "Typical lab velocities", col = "#1f77b4", cex = 0.9)
rect(0.01, 11.5, 0.08, 12.5, col = rgb(1, 0.5, 0.05, 0.5), border = NA)
text(0.06, 10.8, "Typical field velocities", col = "#ff7f0e", cex = 0.9)
