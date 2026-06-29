# Bellmore, J. R., Sergeant, C. J., Bellmore, R. A., Falke, J. A., & Fellman, J. B. (2022). Modeling coho salmon (Oncorhynchus kisutch) population response to streamflow 
# and water temperature extremes. Canadian Journal of Fisheries and Aquatic Sciences, 80(2), 243–260.
# Based on Equation 16 and Table S1 parameters

# Load required libraries
library(ggplot2)

# Define parameters
Survival_max <- 0.5   # Maximum proportion of fish that survive ocean residence
HS_smolt <- 105       # Smolt length at which marine survival is 50% of the maximum
beta_smolt <- 5       # Shape parameter for marine survival
HS_smolt_low <- 94.5  
HS_smolt_high <- 115.5 

# Generate a sequence of smolt lengths 
length_seq <- seq(50, 200, by = 5) 

# Calculate Marine Mortality using Equation 16
mortality_base <- 1 - (Survival_max * (length_seq^beta_smolt)) / ((length_seq^beta_smolt) + (HS_smolt_base^beta_smolt))
mortality_lower <- 1 - (Survival_max * (length_seq^beta_smolt)) / ((length_seq^beta_smolt) + (HS_smolt_low^beta_smolt))
mortality_upper <- 1 - (Survival_max * (length_seq^beta_smolt)) / ((length_seq^beta_smolt) + (HS_smolt_high^beta_smolt))

# Combine into dataframe
plot_df <- data.frame(
  Length = length_seq,
  Base = mortality_base,
  Lower = mortality_lower,
  Upper = mortality_upper
)

# Export the data to a CSV file
write.csv(plot_df, "SmoltLength_MarineMortality.csv", row.names = FALSE)

# Create the plot
ggplot(plot_df, aes(x = Length)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = "Uncertainty Bounds"), alpha = 0.3) +
  geom_line(aes(y = Lower, color = "Lower Limit (HS = 94.5)"), linetype = "dashed", size = 1) +
  geom_line(aes(y = Upper, color = "Upper Limit (HS = 115.5)"), linetype = "dashed", size = 1) +
  geom_line(aes(y = Base, color = "Baseline (HS = 105)"), size = 1.2) +
  scale_fill_manual(values = c("Uncertainty Bounds" = "gray")) +
  scale_color_manual(values = c("Baseline (HS = 105)" = "black", 
                                "Lower Limit (HS = 94.5)" = "blue", 
                                "Upper Limit (HS = 115.5)" = "red")) +
  theme_minimal() +
  labs(
    title = "Bellmore 2022: Coho Smolt Length vs. Marine Mortality",
    x = "Smolt Length (mm)",
    y = "Marine Mortality (Proportion)",
    color = "Curves",
    fill = ""
  ) +
  theme(legend.position = "right")
