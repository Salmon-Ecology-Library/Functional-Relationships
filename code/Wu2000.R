# Wu, F. (2000). Modeling embryo survival affected by sediment deposition into salmonid spawning gravels: 
# Application to flushing flow prescriptions. Water Resources Research, 36(6), 1595–1603
# Wu 2000: Chinook salmon embryo survival vs. specific sediment deposit
# Equation 2, 3, and 4

# Load necessary libraries
library(dplyr)
library(ggplot2)

# ==============================================================================
# 1. Define the Mechanistic Equations from Wu (2000)
# ==============================================================================

# Equation 2: Reduced permeability ratio (K/K0)
calc_k_ratio <- function(sigma, ds_Dg) {
  # pmax ensures the term doesn't drop below 0 before being cubed
  term1 <- 4.54 * (pmax(0.42 - 1.54 * sigma, 0)^3) / ((0.58 + 1.54 * sigma)^2)
  term2 <- 3.66 * (ds_Dg^2) * sigma
  return(term1 + term2)
}

# Equation 3: Apparent velocity (V')
calc_v_prime <- function(k_ratio, h_L1, L2_L1, K2 = 3.0) {
  # Prevent division by zero
  k_ratio <- ifelse(k_ratio <= 0, 1e-10, k_ratio)
  K2_K1 <- 1.0 / k_ratio
  v_prime <- (h_L1 * K2) / (L2_L1 + K2_K1)
  return(v_prime)
}

# Equation 4: Embryo survival percentage (S)
calc_survival <- function(v_prime) {
  v_prime <- ifelse(v_prime <= 1e-10, 1e-10, v_prime)
  log_v <- log10(v_prime)
  s <- -17.6 * (log_v^2) - 39.6 * log_v + 68.7
  # Cap biological bounds between 0% and 100%
  s <- pmax(0, pmin(s, 100))
  return(s)
}

# ==============================================================================
# 2. Function to Generate Data for a Given Set of Parameters
# ==============================================================================

generate_curve_data <- function(curve_id, stressor_val, ds_Dg, h_L1, L2_L1, group_label) {
  # Use 20 points across the sigma range (0 to 0.27) for a smooth curve
  sigma_vals <- seq(0, 0.27, length.out = 20) 
  
  k_rat <- calc_k_ratio(sigma_vals, ds_Dg)
  v_p <- calc_v_prime(k_rat, h_L1, L2_L1)
  surv <- calc_survival(v_p)
  
  data.frame(
    curve.id = curve_id,
    stressor.label = rep("specific_sediment_deposit", length(sigma_vals)),
    stressor.x = round(sigma_vals, 3),
    units.x = rep("proportion", length(sigma_vals)),
    response.label = rep("embryo_survival", length(sigma_vals)),
    response.y = round(surv, 2),
    units.y = rep("percent", length(sigma_vals)),
    stressor.value = rep(stressor_val, length(sigma_vals)),
    Plot_Group = rep(group_label, length(sigma_vals))
  )
}

# ==============================================================================
# 3. Generate Data for Base Model & Sensitivity Curves (Figs 4b, 5b, 6b)
# ==============================================================================

# Base parameters
base_ds_Dg <- 0.07
base_h_L1  <- 0.5
base_L2_L1 <- 35

all_data <- list()

# -- Fig 4b: Sensitivity to Sediment-Gravel Size Ratio (ds/Dg) --
ds_vals <- c(0.03, 0.05, 0.07, 0.09, 0.11)
for(val in ds_vals) {
  c_id <- paste0("size_ratio_", val)
  s_val <- paste0("ds/Dg=", val)
  # Mark base value
  if(val == base_ds_Dg) { c_id <- "base_model_size_ratio"; s_val <- "base (ds/Dg=0.07)" }
  
  all_data[[length(all_data) + 1]] <- generate_curve_data(c_id, s_val, val, base_h_L1, base_L2_L1, "Fig 4b: Size Ratio (ds/Dg)")
}

# -- Fig 5b: Sensitivity to Pressure Head (h/L1) --
h_vals <- c(0.2, 0.3, 0.5, 0.8, 1.5)
for(val in h_vals) {
  c_id <- paste0("pressure_head_", val)
  s_val <- paste0("h/L1=", val)
  # Mark base value
  if(val == base_h_L1) { c_id <- "base_model_pressure_head"; s_val <- "base (h/L1=0.5)" }
  
  all_data[[length(all_data) + 1]] <- generate_curve_data(c_id, s_val, base_ds_Dg, val, base_L2_L1, "Fig 5b: Pressure Head (h/L1)")
}

# -- Fig 6b: Sensitivity to Flow Path Length (L2/L1) --
L2_vals <- c(15, 25, 35, 45, 55)
for(val in L2_vals) {
  c_id <- paste0("flow_path_", val)
  s_val <- paste0("L2/L1=", val)
  # Mark base value
  if(val == base_L2_L1) { c_id <- "base_model_flow_path"; s_val <- "base (L2/L1=35)" }
  
  all_data[[length(all_data) + 1]] <- generate_curve_data(c_id, s_val, base_ds_Dg, base_h_L1, val, "Fig 6b: Flow Path (L2/L1)")
}

# Combine all generated data into a single dataframe
final_df <- do.call(rbind, all_data)

# ==============================================================================
# 4. Export to CSV
# ==============================================================================

export_df <- final_df %>% select(-Plot_Group)

# Save to CSV
write.csv(export_df, "Wu_2000_SRF_Sensitivity_Data.csv", row.names = FALSE)

# ==============================================================================
# 5. Plot the Stressor-Response Curves (Faceted)
# ==============================================================================

srf_plot <- ggplot(final_df, aes(x = stressor.x, y = response.y, color = stressor.value, group = curve.id)) +
  geom_line(size = 1) +
  facet_wrap(~ Plot_Group, ncol = 3) +
  labs(
    title = "Wu (2000): Chinook Embryo Survival vs. Sediment Deposition",
    subtitle = "Sensitivity curves mirroring Figures 4b, 5b, and 6b",
    x = expression(paste("Specific Sediment Deposit (", sigma, ") [proportion]")),
    y = "Embryo Survival (%)",
    color = "Parameter Value"
  ) +
  scale_y_continuous(limits = c(30, 100), breaks = seq(30, 100, 10)) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray40"),
    strip.text = element_text(face = "bold", size = 11),
    axis.title = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    legend.title = element_text(face="bold")
  )

# Display the plot
print(srf_plot)
