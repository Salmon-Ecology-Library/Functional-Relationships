# Beeman J, Juhnke S, Stutzer G, Wright K. Effects of Iron Gate Dam discharge and other factors on the survival and migration of 
# juvenile coho salmon in the lower Klamath River, northern California, 2006-09. Open-File Report 2012-1067. US Geological Survey; 2012.
# https://doi.org/10.3133/ofr20121067.

# Code generated with Gemini 3.1 Pro on 6/4/26 by AH Fullerton
# Prompt:
# Write an R script to reproduce the curves shown in Figure 6 of the attached publication. 
# The curves were generated using coefficients in Table 8 for the 'Release to Shasta River' reach.

# Result
# Because apparent survival represents a probability bounded between 0 and 1, the model uses a logit link function. 
# The predicted survival $\phi$ is calculated as:$$\phi = \frac{\exp(LP)}{1 + \exp(LP)}$$
# Where the Linear Predictor ($LP$) for the Release to Shasta River reach is given by:  
# $$LP = -3.3086 + 0.0268(\text{Discharge}) + 0.2987(\text{Temperature}) + 0.0105(\text{Weight})$$
# Discharge is in hundreds of ft³/s.
# Temperature is in °C.
# Weight is the fish weight at tagging in grams.
# To isolate the effect of specific variables in panels A through D, the unvaried covariates are held constant at their mean values 
# for that reach (found in Appendix G): roughly 31.91 g for weight, 12.33 °C for temperature, and 24.82 hundred ft³/s for discharge. 


# Constants from Table 8 Unstandardized Estimates (Release to Shasta River)
b0 <- -3.3086
b_q <- 0.0268
b_t <- 0.2987
b_w <- 0.0105

# Mean values from Appendix G to hold constant when not varied
mean_w <- 31.91  # Mean fish weight (g)
mean_q <- 24.82  # Mean discharge (hundred ft3/s)
mean_t <- 12.33  # Mean water temperature (C)

# Inverse logit function to convert linear predictor to probability
inv_logit <- function(x) {
  exp(x) / (1 + exp(x))
}

# Define ranges based on observed data minimums and maximums
temp_seq <- seq(7.58, 17.91, length.out = 100)
q_seq <- seq(14.1, 98.8, length.out = 100)

# Set up the 2x2 plotting grid
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 2, 1), oma = c(1, 1, 2, 1))

# ---------------------------------------------------------
# Panel A: Temp vs Survival (Discharge varied, Weight constant)
# ---------------------------------------------------------
lp_A1 <- b0 + b_q * 14.1 + b_t * temp_seq + b_w * mean_w
lp_A2 <- b0 + b_q * 98.8 + b_t * temp_seq + b_w * mean_w

plot(temp_seq, inv_logit(lp_A1), type = "l", col = "black", lty = 1,
     ylim = c(0, 1.2), las = 1, xlab = "Water temperature (C)", 
     ylab = "Predicted apparent survival", main = "A")
lines(temp_seq, inv_logit(lp_A2), col = "red", lty = 3, lwd = 3)
legend("bottomright", legend = c("14.1 hundred ft3/s", "98.8 hundred ft3/s"),
       col = c("black", "red"), lty = c(1, 3), lwd = c(1, 3), bty = "n")

# ---------------------------------------------------------
# Panel B: Temp vs Survival (Weight varied, Discharge constant)
# ---------------------------------------------------------
lp_B1 <- b0 + b_q * mean_q + b_t * temp_seq + b_w * 11.5
lp_B2 <- b0 + b_q * mean_q + b_t * temp_seq + b_w * 130.6

plot(temp_seq, inv_logit(lp_B1), type = "l", col = "black", lty = 1,
     ylim = c(0, 1.2), las = 1, xlab = "Water temperature (C)", ylab = "",
     main = "B")
lines(temp_seq, inv_logit(lp_B2), col = "red", lty = 3, lwd = 3)
legend("bottomright", legend = c("11.5 g", "130.6 g"),
       col = c("black", "red"), lty = c(1, 3), lwd = c(1, 3), bty = "n")

# ---------------------------------------------------------
# Panel C: Discharge vs Survival (Temp varied, Weight constant)
# ---------------------------------------------------------
lp_C1 <- b0 + b_q * q_seq + b_t * 7.6 + b_w * mean_w
lp_C2 <- b0 + b_q * q_seq + b_t * 17.9 + b_w * mean_w

plot(q_seq, inv_logit(lp_C1), type = "l", col = "black", lty = 1,
     ylim = c(0, 1.2), las = 1, xlab = "Discharge (hundred ft3/s)", 
     ylab = "Predicted apparent survival", main = "C")
lines(q_seq, inv_logit(lp_C2), col = "red", lty = 3, lwd = 3)
legend("bottomright", legend = c("7.6 C", "17.9 C"),
       col = c("black", "red"), lty = c(1, 3), lwd = c(1, 3), bty = "n")

# ---------------------------------------------------------
# Panel D: Discharge vs Survival (Weight varied, Temp constant)
# ---------------------------------------------------------
lp_D1 <- b0 + b_q * q_seq + b_t * mean_t + b_w * 11.5
lp_D2 <- b0 + b_q * q_seq + b_t * mean_t + b_w * 130.6

plot(q_seq, inv_logit(lp_D1), type = "l", col = "black", lty = 1,
     ylim = c(0, 1.2), las = 1, xlab = "Discharge (hundred ft3/s)", ylab = "",
     main = "D")
lines(q_seq, inv_logit(lp_D2), col = "red", lty = 3, lwd = 3)
legend("bottomright", legend = c("11.5 g", "130.6 g"),
       col = c("black", "red"), lty = c(1, 3), lwd = c(1, 3), bty = "n")
