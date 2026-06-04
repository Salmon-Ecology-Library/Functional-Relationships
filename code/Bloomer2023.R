# Bloomer J, Anderson JJ, Sear D, Greene S, Gantner D, Hanson C. Gastrulation and hatch as critical thermal windows for salmonid embryo development. 
  # River Research and Applications 2023; 39:46-83.DOI: 10.1002/rra.4066

# Code generated via interaction with Gemini 3.1 Pro on 5/22/26 by AH Fullerton

library(tidyr)
library(dplyr)
library(ggplot2)


# 1. Define treatments and parameters (Study A from Table 2)
T_vec <- c(12.5, 14.5, 16.5, 18.5, 20.5)

# Coefficients and their Standard Errors for Study A
coefs <- data.frame(
  Stage = c("C", "G", "O", "H"),
  a = c(-8.17, -16.10, -10.75, -23.31),
  b = c(-0.020, 0.029, -0.005, 0.037),
  c = c(0.198, 0.601, 0.390, 0.555),
  # Standard errors extracted from Table 2
  SE_a = c(1.664, 1.426, 0.998, 1.555),
  SE_b = c(0.024, 0.009, 0.003, 0.004),
  SE_c = c(0.136, 0.070, 0.047, 0.051) 
)

# 2. Simulate day-by-day progression
results_df <- expand_grid(
  T_val = T_vec,
  Day = 1:35 
) %>%
  mutate(DD_val = Day * T_val) %>%
  filter(DD_val <= 370) %>% 
  mutate(
    Stage = case_when(
      DD_val <= 50 ~ "C",
      DD_val <= 125 ~ "G",
      DD_val <= 250 ~ "O",
      TRUE ~ "H"
    )
  ) %>%
  left_join(coefs, by = "Stage") %>%
  
  # 3. Apply equations and calculate error bounds
  mutate(
    # Base daily hazard rate
    log_r = a + (b * DD_val) + (c * T_val),
    r_val = exp(log_r),
    
    # Error propagation for log(r)
    SE_log_r = sqrt(SE_a^2 + (DD_val * SE_b)^2 + (T_val * SE_c)^2),
    
    # Calculate 95% Confidence Intervals for daily hazard rate (using 1.96)
    r_lower = exp(log_r - 1.96 * SE_log_r),
    r_upper = exp(log_r + 1.96 * SE_log_r)
  ) %>%
  
  # 4. Integrate Equations for Cumulative Mortality and its bounds
  group_by(T_val) %>%
  arrange(Day) %>%
  mutate(
    # Accumulate the hazard rates
    cumulative_hazard = cumsum(r_val),
    cumulative_hazard_lower = cumsum(r_lower),
    cumulative_hazard_upper = cumsum(r_upper),
    
    # Convert to Cumulative Mortality
    CM = 1 - exp(-cumulative_hazard),
    CM_lower = 1 - exp(-cumulative_hazard_lower),
    CM_upper = 1 - exp(-cumulative_hazard_upper)
  ) %>%
  ungroup()

ggplot(results_df, aes(x = DD_val, y = CM, color = as.factor(T_val), fill = as.factor(T_val))) +
  
  # Add the 95% confidence interval ribbons
  geom_ribbon(aes(ymin = CM_lower, ymax = CM_upper), alpha = 0.15, color = NA) +
  
  # Base lines and points
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  
  # Add vertical dotted lines for stage boundaries 
  geom_vline(xintercept = c(50, 125, 250), linetype = "dotted", color = "gray50") +
  
  # Annotate the stages
  annotate("text", x = 25, y = 1.05, label = "C", fontface = "bold") +
  annotate("text", x = 87.5, y = 1.05, label = "G", fontface = "bold") +
  annotate("text", x = 187.5, y = 1.05, label = "O", fontface = "bold") +
  annotate("text", x = 310, y = 1.05, label = "H", fontface = "bold") +
  
  # Formatting limits and labels
  scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, by = 0.2)) +
  labs(
    x = "Degree day (D)",
    y = "Cumulative mortality (CM)",
    color = "Treatment",
    fill = "Treatment" # Ensure the legend combines both color and fill
  ) +
  theme_classic() +
  theme(legend.position = c(0.15, 0.75))

write.csv(results_df[, c("T_val", "DD_val", "Stage", "CM", "CM_lower", "CM_upper")], "Bloomer2023_data.csv", row.names = F)

