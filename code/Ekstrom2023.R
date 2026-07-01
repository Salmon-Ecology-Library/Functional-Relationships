# Ekstrom et al. 2023 https://doi.org/10.1038/s41598-023-47713-5
# Code generated with Gemini 3.1 Pro on 7/1/26 using a custom GEM 'SRF Data Extraction' by AH Fullerton

# Load required library for plotting
library(ggplot2)
library(gridExtra)

# 1. Create the dataset
data <- data.frame(
  curve.id = c(rep("Fig2F_Sham", 6), rep("Fig2H_Sham", 6)),
  stressor.label = rep("Temperature", 12),
  stressor.x = c(15, 19, 20, 21, 22, 23, 15, 19, 20, 21, 22, 23),
  units.x = rep("degC", 12),
  response.label = c(rep("O2_consumption_rate", 6), rep("Heart_rate", 6)),
  response.y = c(463, 560, 560, 600, 680, 780, 80, 95, 98, 100, 115, 120),
  units.y = c(rep("mg_O2_h-1_kg-1", 6), rep("beats_min-1", 6))
)

# 2. Export the data to a CSV file
write.csv(data, "data/Ekstrom_2023_Fig2_Sham_Data.csv", row.names = FALSE)

# 3. Generate Plot for Panel F (O2 Consumption Rate)
plot_F <- ggplot(subset(data, curve.id == "Fig2F_Sham"), aes(x = stressor.x, y = response.y)) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_point(color = "darkred", size = 3) +
  scale_x_continuous(breaks = c(15, 19, 20, 21, 22, 23, 24)) +
  scale_y_continuous(limits = c(0, 1000)) +
  labs(title = "Panel F: O2 Consumption Rate",
       subtitle = "Sham Operated",
       x = "Temperature (°C)",
       y = "O2 consumption rate (mg O2 h-1 kg-1)") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))

# 4. Generate Plot for Panel H (Heart Rate)
plot_H <- ggplot(subset(data, curve.id == "Fig2H_Sham"), aes(x = stressor.x, y = response.y)) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_point(color = "darkred", size = 3) +
  scale_x_continuous(breaks = c(15, 19, 20, 21, 22, 23, 24)) +
  scale_y_continuous(limits = c(0, 160)) +
  labs(title = "Panel H: Heart Rate",
       subtitle = "Sham Operated",
       x = "Temperature (°C)",
       y = "Heart rate (beats min-1)") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold"))

# 5. Display the plots side-by-side
grid.arrange(plot_F, plot_H, ncol = 2)
