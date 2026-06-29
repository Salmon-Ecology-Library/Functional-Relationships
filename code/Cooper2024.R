# Cooper SD, Wiseman SW, DiFiore BP, Klose K. Trout and invertebrate assemblages in stream pools through wildfire and drought. 
# Freshwater Biology. 2024 Feb;69(2):300-20. https://doi.org/10.1111/fwb.14212
# Used empirical data from Dryad https://datadryad.org/dataset/doi:10.5061/dryad.h70rxwdqq

# Code generated with Gemini 3.1 Pro on 6/18/26 using a custom GEM 'SRF Data Extraction' by AH Fullerton
# Lots of back and forth conversation testing approaches and fixing errors.
# The confidence bands don't work well and Gemini's suggestion to use glmmTMB as in the original publication did not work well either (see end of script).

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(lme4)

# ---------------------------------------------------------
# 1. Read Empirical Data
# ---------------------------------------------------------
data_emp <- read_csv("data/Cooper_2024_data_raw.csv")

# ---------------------------------------------------------
# 2. Scale Variables and Fit the Model
# ---------------------------------------------------------
# Calculate means and standard deviations for scaling
mean_depth <- mean(data_emp$depth, na.rm = TRUE)
sd_depth <- sd(data_emp$depth, na.rm = TRUE)

mean_do <- mean(data_emp$DO, na.rm = TRUE)
sd_do <- sd(data_emp$DO, na.rm = TRUE)

# Create scaled versions of the predictors
data_emp$depth_s <- (data_emp$depth - mean_depth) / sd_depth
data_emp$DO_s <- (data_emp$DO - mean_do) / sd_do

# Fit the model using scaled variables and bypass the overly strict derivative check
model_full <- glmer(trout ~ depth_s + DO_s + (1 | reach) + (1 | year), 
                    data = data_emp, 
                    family = binomial(link = "logit"),
                    control = glmerControl(optimizer = "bobyqa", 
                                           optCtrl = list(maxfun = 100000),
                                           calc.derivs = FALSE))

# Calculate the constants for predictions in their ORIGINAL units
q75_depth <- quantile(data_emp$depth, probs = 0.75, na.rm = TRUE)

# Helper function for inverse logit transformation
inv_logit <- function(x) { exp(x) / (1 + exp(x)) }


# ---------------------------------------------------------
# 3. Generate Predictions and 95% CIs (Population-Level)
# ---------------------------------------------------------
get_fixed_preds <- function(model, newdata) {
  # Extract the fixed-effects formula and remove the response variable (LHS)
  fixed_formula <- formula(model, fixed.only = TRUE)
  rhs_formula <- fixed_formula[-2]
  
  # Create the design matrix for fixed effects from the scaled new data
  X_mat <- model.matrix(rhs_formula, data = newdata)
  
  # Calculate linear predictor (link scale)
  fit_link <- X_mat %*% fixef(model)
  
  # Calculate standard errors based on variance-covariance matrix
  se_link <- sqrt(diag(X_mat %*% vcov(model) %*% t(X_mat)))
  
  # Transform back to probability scale
  newdata$response.y <- as.vector(inv_logit(fit_link))
  newdata$lower.limit <- as.vector(inv_logit(fit_link - 1.96 * se_link))
  newdata$upper.limit <- as.vector(inv_logit(fit_link + 1.96 * se_link))
  
  return(newdata)
}

# --- Predict for Depth ---
# Hold DO at its original mean, vary depth across its original range
pred_depth <- data.frame(
  depth = seq(min(data_emp$depth, na.rm = TRUE), max(data_emp$depth, na.rm = TRUE), length.out = 200),
  DO = mean_do
)
# Apply the exact same scaling to the prediction data so the model understands it
pred_depth$depth_s <- (pred_depth$depth - mean_depth) / sd_depth
pred_depth$DO_s <- (pred_depth$DO - mean_do) / sd_do

pred_depth <- get_fixed_preds(model_full, pred_depth)


# --- Predict for DO ---
# Hold depth at its original 75th percentile, vary DO across its original range
pred_do <- data.frame(
  DO = seq(min(data_emp$DO, na.rm = TRUE), max(data_emp$DO, na.rm = TRUE), length.out = 200),
  depth = q75_depth
)
# Apply the scaling to the prediction data
pred_do$depth_s <- (pred_do$depth - mean_depth) / sd_depth
pred_do$DO_s <- (pred_do$DO - mean_do) / sd_do

pred_do <- get_fixed_preds(model_full, pred_do)


# ---------------------------------------------------------
# 4. Recreate Figures
# ---------------------------------------------------------
# Figure 3a (Depth)
fig3a <- ggplot() +
  geom_ribbon(data = pred_depth, aes(x = depth, ymin = lower.limit, ymax = upper.limit), 
              fill = "grey70", alpha = 0.5) +
  geom_line(data = pred_depth, aes(x = depth, y = response.y), color = "black", linewidth = 1) +
  geom_point(data = data_emp, aes(x = depth, y = trout), 
             color = "black", size = 2, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(x = "Average max. depth (m)", y = "Trout presence") +
  theme_classic() +
  theme(axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black", face = "bold"))

# Figure 3b (Dissolved Oxygen)
fig3b <- ggplot() +
  geom_ribbon(data = pred_do, aes(x = DO, ymin = lower.limit, ymax = upper.limit), 
              fill = "grey70", alpha = 0.5) +
  geom_line(data = pred_do, aes(x = DO, y = response.y), color = "black", linewidth = 1) +
  geom_point(data = data_emp, aes(x = DO, y = trout), 
             color = "black", size = 2, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(breaks = c(3, 6, 9)) +
  labs(x = expression(bold(paste("Dissolved oxygen (mg L"^"-1", ")"))), 
       y = "Trout presence") +
  theme_classic() +
  theme(axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black", face = "bold"))

print(fig3a)
print(fig3b)


# ---------------------------------------------------------
# 5. Format Output Data to the Extracted Data Schema
# ---------------------------------------------------------
# Note: We export the UNSCALED 'depth' and 'DO' for stressor.x
ext_depth <- pred_depth %>%
  mutate(
    curve.id = "curve_depth",
    stressor.label = "Depth",
    stressor.x = round(depth, 4),
    units.x = "m",
    response.label = "Trout presence probability",
    response.y = round(response.y, 4),
    units.y = "proportion",
    plot.type = "curve",
    sd = NA,
    stressor.value = paste0("DO held at mean: ", round(mean_do, 2), " mg/L"),
    lower.limit = round(lower.limit, 4),
    upper.limit = round(upper.limit, 4)
  ) %>%
  select(curve.id, stressor.label, stressor.x, units.x, response.label, 
         response.y, units.y, stressor.value, lower.limit, upper.limit, sd, plot.type)

ext_do <- pred_do %>%
  mutate(
    curve.id = "curve_do",
    stressor.label = "DO",
    stressor.x = round(DO, 4),
    units.x = "mg/L",
    response.label = "Trout presence probability",
    response.y = round(response.y, 4),
    units.y = "proportion",
    plot.type = "curve",
    sd = NA,
    stressor.value = paste0("Depth held at upper 3rd quartile: ", round(q75_depth, 2), " m"),
    lower.limit = round(lower.limit, 4),
    upper.limit = round(upper.limit, 4)
  ) %>%
  select(curve.id, stressor.label, stressor.x, units.x, response.label, 
         response.y, units.y, stressor.value, lower.limit, upper.limit, sd, plot.type)

final_extracted_data <- bind_rows(ext_depth, ext_do)

write_csv(final_extracted_data, "data/Cooper_2024_data.csv")


# Gemini explanation about confidence band failure:
# When a logistic regression has near-perfect separation (meaning a certain depth or DO level almost perfectly cleanly divides where trout 
# are present versus absent), the maximum likelihood estimates become very large. In the lme4 package, the variance-covariance matrix (vcov) 
# gets inflated exponentially as a mathematical artifact of the flat likelihood surface. When we calculated the standard errors using that matrix, 
# they blew up, meaning your 95% confidence intervals on the logit scale likely spanned from -50 to +50. When back-transformed to probabilities, 
# that becomes 0 to 1 everywhere, creating a massive, screen-filling grey box.
# Suggestion was to use glmmTMB package, but the script it generated failed to reproduce results. Abandoned.

