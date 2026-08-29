# McNeil, W. J. (1969). Survival of pink and chum salmon eggs and alevins. 
# Salmon and trout in streams. HR McMillan Lectures in Fisheries, University of British Columbia, Vancouver, 101-117.
# McNeil 1969: Eggs in spawning grounds vs. potential egg deposition
# equation: R = L(1-e^-E/L) (Figure 5)

# Load necessary library for plotting
library(ggplot2)

# 1. Generate Data
L <- 2 # Asymptotic limit (thousands of eggs per m^2) based on visual approximation of Fig 5
E_potential <- seq(0, 7, by = 0.5) # Potential egg deposition (thousands per m^2)

# Apply the mathematical model for redd superimposition
R_remaining <- L * (1 - exp(-E_potential / L))

# Create dataframe
srf4_data <- data.frame(
  curve.id = rep("McNeil_1969_Fig6", length(E_potential)),
  stressor.label = rep("potential_egg_deposition", length(E_potential)),
  stressor.x = E_potential,
  units.x = rep("thousands_eggs_per_m2", length(E_potential)),
  response.label = rep("eggs_remaining", length(E_potential)),
  response.y = round(R_remaining, 3),
  units.y = rep("thousands_eggs_per_m2", length(E_potential))
)

# 2. Export to CSV
write.csv(srf4_data, "McNeil_1969_ReddSuperimposition.csv", row.names = FALSE)

# 3. Generate Plot
ggplot(srf4_data, aes(x = stressor.x, y = response.y)) +
  geom_line(color = "red", size = 1) +
  geom_point(size = 2, color = "darkred") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") + # 100% survival reference line
  theme_minimal() +
  labs(
    title = "Eggs Remaining vs. Potential Egg Deposition",
    subtitle = "Dashed line represents theoretical 100% recruitment (y = x)",
    x = "Potential Egg Deposition (thousands per m²)",
    y = "Eggs Remaining at End of Spawning (thousands per m²)"
  )
