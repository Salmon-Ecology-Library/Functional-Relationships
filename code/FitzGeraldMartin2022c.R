# FitzGerald, A. M., & Martin, B. T. (2022). Quantification of thermal impacts across 
# freshwater life stages to improve temperature management for anadromous salmonids. 
# Conservation Physiology, 10(1), coac013.
# FitzGerald et al. 2022: Chinook survival to smolting vs rearing temperature

# Load libraries
library(ggplot2)

# Growth parameters
d_param <- 0.415
g_param <- 0.315
Tl <- 1.833
Tu <- 24.918
Tcorr <- 3.0
a <- 0.338

# Mortality parameters
Xw <- 0.00753
f <- -0.27

# Function to iteratively calculate survival until smolt size
calc_smolt_survival <- function(temp, food) {
  Te <- temp + Tcorr
  
  # Check if temperature is outside thermal limits
  if (Te <= Tl || Te >= Tu) {
    return(0.0)
  }
  
  # Calculate specific growth rate
  omega <- food * d_param * (Te - Tl) * (1 - exp(g_param * (Te - Tu)))
  
  if (omega <= 0) {
    return(0.0)
  }
  
  # Initialize mass (M) and survival (S) based on emergent fry
  M <- 0.46
  S <- 1.0
  days <- 0
  
  # Iterate daily until smolt size is reached (cap at 2000 days to prevent infinite loops)
  while (M < 6.0 && days < 2000) {
    M <- M + (omega / 100.0) * (M^(1 - a))
    mu <- Xw * (M^f)
    S <- S * exp(-mu)
    days <- days + 1
  }
  
  if (M >= 6.0) {
    return(S)
  } else {
    return(0.0)
  }
}

# Define stressor ranges and curve groupings
temperatures <- seq(5, 25, by = 1)
food_levels <- c(0.3, 0.5, 0.65, 0.8, 1) # 0.65 is the standard field level cited

results <- list()

for (food in food_levels) {
  for (temp in temperatures) {
    survival_prop <- calc_smolt_survival(temp, food)
    
    # Append to results matching the CSV template
    results[[length(results) + 1]] <- data.frame(
      curve.id = paste0("c_smolt_food", food),
      stressor.label = "temperature",
      stressor.x = temp,
      units.x = "degC",
      response.label = "survival",
      response.y = round(survival_prop, 3),
      units.y = "proportion",
      stressor.value = paste0("food_", food),
      stringsAsFactors = FALSE
    )
  }
}

# Combine and write to CSV
srf3_data <- do.call(rbind, results)
write.csv(srf3_data, "SRF3_Juvenile_Smolting_Data.csv", row.names = FALSE)

# Convert stressor.value to a factor to order the legend logically
srf3_data$stressor.value <- factor(srf3_data$stressor.value, 
                                   levels = paste0("food_", rev(food_levels)))

# Plot the data
srf3_plot <- ggplot(srf3_data, aes(x = stressor.x, y = response.y, color = stressor.value)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "Juvenile Survival to Smolting (FitzGerald et al. 2022)",
    subtitle = "Survival likelihood based on temperature and food availability",
    x = "Temperature (°C)",
    y = "Survival (Proportion)",
    color = "Food Ratio"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

print(srf3_plot)
