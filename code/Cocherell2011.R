# Cocherell, S. A., Cocherell, D. E., Jones, G. J., Miranda, J. B., Thompson, L. C., Cech, J. J. Jr., & Klimley, A. P. (2011). 
# Rainbow trout Oncorhynchus mykiss energetic responses to pulsed flows in the American River, California, assessed by electromyogram telemetry. 
# Environmental Biology of Fishes, 90(1), 29-41. 
# https://doi.org/10.1007/s10641-010-9714-x 

# Code generated with Gemini 3.1 Pro on 6/12/26 using a custom GEM 'SRF Data Extraction' by AH Fullerton
# Context from Gemini: "Because I cannot digitally extract the exact coordinates of the hundreds of overlapping data points directly from the 
# PDF image, I have written an R script that simulates a dataset mirroring the visual distribution of Figure 7a and the specific statistical 
# constraints provided by the authors in the text.

# Load required library
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Set seed for reproducible data simulation
set.seed(123)

# 1. Simulate the Data
# ---------------------------------------------------------
# Simulate data for flows < 44 m3/s
# Mean ~26.6 cm/s, higher variance. Groups 1, 2, and 3 present.
n_low <- 180 # Approximate number of points below threshold
x_low_g1 <- runif(n_low/3, 5, 44)
y_low_g1 <- abs(rnorm(n_low/3, mean = 28, sd = 22))

x_low_g2 <- runif(n_low/3, 5, 44)
y_low_g2 <- abs(rnorm(n_low/3, mean = 22, sd = 15))

x_low_g3 <- runif(n_low/3, 5, 44)
y_low_g3 <- abs(rnorm(n_low/3, mean = 32, sd = 25))

# Simulate data for flows > 44 m3/s
# Mean ~17.5 cm/s, lower variance. Only Groups 1 and 2 present.
n_high <- 60 # Approximate number of points above threshold
x_high_g1 <- runif(n_high/2, 45, 85)
y_high_g1 <- abs(rnorm(n_high/2, mean = 18, sd = 12))

x_high_g2 <- runif(n_high/2, 45, 85)
y_high_g2 <- abs(rnorm(n_high/2, mean = 10, sd = 5))

# 2. Structure into the Required CSV Format
# ---------------------------------------------------------
df_g1 <- data.frame(
  curve.id = "Group 1",
  stressor.label = "river_discharge",
  stressor.x = c(x_low_g1, x_high_g1),
  units.x = "m3/s",
  response.label = "swimming_speed",
  response.y = c(y_low_g1, y_high_g1),
  units.y = "cm/s",
  plot.type = "scatter"
)

df_g2 <- data.frame(
  curve.id = "Group 2",
  stressor.label = "river_discharge",
  stressor.x = c(x_low_g2, x_high_g2),
  units.x = "m3/s",
  response.label = "swimming_speed",
  response.y = c(y_low_g2, y_high_g2),
  units.y = "cm/s",
  plot.type = "scatter"
)

df_g3 <- data.frame(
  curve.id = "Group 3",
  stressor.label = "river_discharge",
  stressor.x = x_low_g3,
  units.x = "m3/s",
  response.label = "swimming_speed",
  response.y = y_low_g3,
  units.y = "cm/s",
  plot.type = "scatter"
)

# Combine all groups
final_df <- rbind(df_g1, df_g2, df_g3)

# Cap unrealistic high outliers and negative values to match bounds of Fig 7a
final_df$response.y[final_df$response.y > 115] <- 115
final_df$response.y[final_df$response.y < 0] <- 0

# 3. Export to CSV
# ---------------------------------------------------------
write.csv(final_df, "data/ cocherell_2011_fig7a_simulated.csv", row.names = FALSE)
cat("Data successfully exported to 'cocherell_2011_fig7a_simulated.csv'\n")

# 4. Replicate Figure 7a
# ---------------------------------------------------------
ggplot(final_df, aes(x = stressor.x, y = response.y, shape = curve.id)) +
  geom_point(size = 3, alpha = 0.7, color = "black") +
  # Match original plot symbols: Group 1 (Solid Circle), Group 2 (Solid Square), Group 3 (Open Circle)
  scale_shape_manual(values = c("Group 1" = 16, "Group 2" = 15, "Group 3" = 1)) +
  scale_x_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 10)) +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 20)) +
  labs(
    title = "Figure 7A: Swimming Speed vs. River Discharge (Simulated)",
    x = "River Discharge (m3/s)",
    y = "Swimming Speed (cm/s)"
  ) +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.85, 0.85),
    panel.border = element_rect(colour = "black", fill=NA, size=1)
  )