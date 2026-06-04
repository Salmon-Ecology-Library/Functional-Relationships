# Bell HN 2022. Assessment of the Effects of Thiamine Deficiency on the Survival, Physiology, and Behavior of Early 
# Life-Stage Winter-Run Chinook Salmon. Thesis, University of California Davis. https://escholarship.org/uc/item/465026qq

# Code generated with Gemini 3.1 Pro on 6/1/26 by AH Fullerton
# Prompt:
# Write an R script to produce the curve in Figure 2 of this publication, which uses a binomial logistic regression model with a 
# logit link function to predict the proportion of fish whose parents were untreated with thiamine compounds surviving versus 
# egg total thiamine concentration (nmol/g).

# Result
# Because the exact survival proportions for each individual family are not tabulated in the provided document, 
# this script extracts the 27 known total egg thiamine (TTH) concentrations for untreated females directly from Table 1. 
# It then simulates the survival data points by using the document's reported logistic regression parameters—specifically 
# the $2.7 \text{ nmol/g}$ EC50 threshold and the $4.97 \text{ nmol/g}$ EC95 threshold —to approximate the scatter points 
# seen in the publication.  

setwd("~/GitHub/SRF_Extraction/Bell2022")
# Load necessary libraries
library(ggplot2)
library(dplyr)

# 1. Input the known data
# Total Thiamine (TTH) values (nmol/g) for untreated females extracted from Table 1
tth_values <- c(5.4, 25.1, 5.6, 3.0, 4.5, 10.6, 3.4, 3.8, 4.1, 3.4,
                5.3, 4.7, 4.1, 2.7, 3.5, 3.6, 5.2, 4.3, 4.0, 3.1,
                4.9, 5.3, 2.8, 3.5, 3.7, 2.4, 3.9)

# 2. Approximate the survival data
# Since the exact survival percentages per family are not in the text,
# we use the reported EC50 (2.7) and EC95 (4.97) to rebuild the logistic model.
# Solving for logistic parameters (beta_1 = 1.297, beta_0 = -3.502)
set.seed(42) # Set seed for reproducibility of random noise
logit_p <- -3.502 + (1.297 * tth_values)
expected_survival <- 1 / (1 + exp(-logit_p))
# Add some random noise to mimic the real-world scatter observed in Figure 2
simulated_survival <- pmax(0, pmin(1, expected_survival + rnorm(length(tth_values), 0, 0.2)))
# Create the data frame
df <- data.frame(TTH = tth_values, Survival = simulated_survival)


# Use data extracted from Figure 2 using Web Plot Digitizer instead:
df <- read.csv("Bell2022_data.csv")
  

# 3. Plot the data
ggplot(df, aes(x = TTH, y = Survival)) +
  # Add the scatter points (open circles like in Figure 2)
  geom_point(shape = 1, size = 2, color = "black") +
  
  # Fit and plot the binomial logistic regression curve with a 95% CI shaded region
  geom_smooth(method = "glm",
              method.args = list(family = binomial(link = "logit")),
              level = 0.25,  # not sure what error the shaded region represents in the thesis
              color = "black",
              fill = "steelblue", 
              alpha = 0.4) +
  
  # Add the vertical dotted line for EC95 at 4.97 nmol/g
  geom_vline(xintercept = 4.97, linetype = "dotted", color = "black", linewidth = 0.8) +
  
  # Format axes to match Figure 2's scales
  scale_x_continuous(limits = c(0, 15), breaks = c(0, 5, 10, 15)) +
  scale_y_continuous(limits = c(-0.05, 1.05), breaks = seq(0, 1, 0.25)) +
  
  # Add labels
  labs(x = "Total Egg Thiamine (nmol/g)",
       y = "Proportion of Fish Surviving Post-Experiment\n(80 - 120 dpf)") +
  
  # Apply a clean, classic theme
  theme_classic() +
  theme(
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 12),
    axis.ticks = element_line(color = "black")
  )
