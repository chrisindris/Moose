# Load necessary library
library(tidyverse)
library(nlme)

# Define the CSV file path
csv_file <- "threemodels.csv"

# Read the CSV file
data <- threemodels
data$model <- as.factor(data$model)
#data$Threshold <- as.factor(data$Threshold)

# View the structure of the dataset to confirm column names and types
str(data)

# ----==========----- IoU=0.5 ----==========-----

# Fit a linear regression model using `IoU=0.50` as the response variable.
# All other columns are used as predictors.
model_additive_poly <- lm(`IoU.0.50` ~ model + poly(Threshold, 3) + Overlap, data = data)
model_interaction_poly <- lm(`IoU.0.50` ~ model + poly(Threshold, 3) * Overlap, data = data)
model_interaction_poly_50 <- model_interaction_poly

anova(model_additive_poly, model_interaction_poly)

# Display a summary of the regression results
summary(model_interaction_poly)


setEPS()                                    # ensure EPS not generic PostScript
postscript("./mlr_threemodels_goodcopy_IoU05_diagnostics.eps",            # output filename
           width = 7, height = 7,           # size in inches
           paper = "special", onefile = FALSE)  # EPS-compliant settings

par(mfrow = c(2, 2))        # 2×2 panel layout
plot(model_interaction_poly) # draws the 4 diagnostic panels
par(mfrow = c(1, 1))        # restore default
dev.off()                    # **must** close the device


res_vs_fit <- data.frame(
  id         = rownames(model_interaction$model),   # keeps track of which row
  fitted     = fitted(model_interaction),           # same as predict(mod)
  residual   = resid(model_interaction)             # same as residuals(mod)
)

res_vs_fit

# Important note: in the residuals vs. fit, it seems like the residuals are negative exactly when the threshold is 0.1 and 1.0.




# ---------- Information criteria ----------
AIC(model_additive_poly, model_interaction_poly)      # ΔAIC > 2 favours the lower-AIC model
BIC(model_additive_poly, model_interaction_poly) # verdict: keep the interaction model

# ---------- Cross-validated RMSE ----------
library(caret)          # install.packages("caret")  if needed
set.seed(2025)

ctrl <- trainControl(method = "cv", number = 10)

rmse_add <- train(IoU.0.50 ~ model + poly(Threshold, 3) + Overlap,
                  data = data, method = "lm",
                  metric = "RMSE", trControl = ctrl)$results$RMSE

rmse_int <- train(IoU.0.50 ~ model + poly(Threshold, 3) * Overlap,
                  data = data, method = "lm",
                  metric = "RMSE", trControl = ctrl)$results$RMSE

c(RMSE_additive = rmse_add, RMSE_interaction = rmse_int)
# verdict: again, go for the interaction model


library(ggplot2)
library(ggeffects)       # install.packages("ggeffects")

# Get predictions for each Overlap × Threshold combination
pred_df <- ggpredict(model_interaction_poly, terms = c("Threshold [all]", "Overlap"))

# numeric version of overlap (e.g. 0, 0.1, 0.3)
data$Overlap_num  <- as.numeric(as.character(data$Overlap))
pred_df$group_num <- as.numeric(as.character(pred_df$group))

plot_IoU05_predictions <- ggplot(pred_df, aes(x = x, y = predicted, colour = group_num)) +
  scale_colour_viridis_c(name = "Overlap", option = "D") +
  geom_line(aes(group = group_num), size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high,
                  fill = group_num,  group = group_num),   # <- added group
              alpha = 0.20, colour = NA) +
  scale_fill_viridis_c(name = "Overlap", option = "D") +
  geom_point(data = data,
             aes(x = Threshold, y = IoU.0.50, colour = Overlap_num),
             position = position_jitter(width = 0.01, height = 0),
             size = 1.5, alpha = 0.8) +
  labs(x = "Threshold", y = "Predicted mAP@0.5") +
  theme_minimal()

print(plot_IoU05_predictions)
ggsave("./mlr_threemodels_goodcopy_IoU05_predictions.eps",
       plot = plot_IoU05_predictions,
       device = cairo_ps,
       dpi = 300,
       units = 'px',
       width = 1360,
       height = 846)
dev.off()




# ----==========----- IoU=0.5:0.95 ----==========-----

# Fit a linear regression model using `IoU=0.50.0.95` as the response variable.
# All other columns are used as predictors.
model_additive_poly <- lm(`IoU.0.50.0.95` ~ model + poly(Threshold, 3) + Overlap, data = data)
model_interaction_poly <- lm(`IoU.0.50.0.95` ~ model + poly(Threshold, 3) * Overlap, data = data)
model_interaction_poly_95 <- model_interaction_poly

anova(model_additive_poly, model_interaction_poly)

# Display a summary of the regression results
summary(model_interaction_poly)

# Base R: write lm diagnostics to an EPS file
setEPS()                                    # ensure EPS not generic PostScript
postscript("./mlr_threemodels_goodcopy_IoU0595_diagnostics.eps",            # output filename
           width = 7, height = 7,           # size in inches
           paper = "special", onefile = FALSE)  # EPS-compliant settings

par(mfrow = c(2, 2))        # 2×2 panel layout
plot(model_interaction_poly) # draws the 4 diagnostic panels
par(mfrow = c(1, 1))        # restore default
dev.off()                    # **must** close the device

res_vs_fit <- data.frame(
  id         = rownames(model_interaction$model),   # keeps track of which row
  fitted     = fitted(model_interaction),           # same as predict(mod)
  residual   = resid(model_interaction)             # same as residuals(mod)
)

res_vs_fit

# Important note: in the residuals vs. fit, it seems like the residuals are negative exactly when the threshold is 0.1 and 1.0.




# ---------- Information criteria ----------
AIC(model_additive_poly, model_interaction_poly)      # ΔAIC > 2 favours the lower-AIC model
BIC(model_additive_poly, model_interaction_poly) # verdict: keep the interaction model

# ---------- Cross-validated RMSE ----------
library(caret)          # install.packages("caret")  if needed
set.seed(2025)

ctrl <- trainControl(method = "cv", number = 10)

rmse_add <- train(IoU.0.50.0.95 ~ model + poly(Threshold, 3) + Overlap,
                  data = data, method = "lm",
                  metric = "RMSE", trControl = ctrl)$results$RMSE

rmse_int <- train(IoU.0.50.0.95 ~ model + poly(Threshold, 3) * Overlap,
                  data = data, method = "lm",
                  metric = "RMSE", trControl = ctrl)$results$RMSE

c(RMSE_additive = rmse_add, RMSE_interaction = rmse_int)
# verdict: again, go for the interaction model


library(ggplot2)
library(ggeffects)       # install.packages("ggeffects")

# Get predictions for each Overlap × Threshold combination
pred_df <- ggpredict(model_interaction_poly, terms = c("Threshold [all]", "Overlap"))

# numeric version of overlap (e.g. 0, 0.1, 0.3)
data$Overlap_num  <- as.numeric(as.character(data$Overlap))
pred_df$group_num <- as.numeric(as.character(pred_df$group))

plot_IoU0595_predictions <- ggplot(pred_df, aes(x = x, y = predicted, colour = group_num)) +
  scale_colour_viridis_c(name = "Overlap", option = "D") +
  geom_line(aes(group = group_num), size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high,
                  fill = group_num,  group = group_num),   # <- added group
              alpha = 0.20, colour = NA) +
  scale_fill_viridis_c(name = "Overlap", option = "D") +
  geom_point(data = data,
             aes(x = Threshold, y = IoU.0.50.0.95, colour = Overlap_num),
             position = position_jitter(width = 0.01, height = 0),
             size = 1.5, alpha = 0.8) +
  labs(x = "Threshold", y = "Predicted mAP@0.5:0.95") +
  theme_minimal()

plot(plot_IoU0595_predictions)
ggsave("./mlr_threemodels_goodcopy_IoU0595_predictions.eps",
       plot = plot_IoU0595_predictions,
       device = cairo_ps,
       dpi = 300,
       units = 'px',
       width = 1360,
       height = 846)
dev.off()

# --------------------------------------------------------------------------------------
# ---------------------------- Ineffective simpler versions ----------------------------
# --------------------------------------------------------------------------------------


# ----==========----- If we use a strictly simple additive model ----==========-----
model_additive <- lm(`IoU.0.50` ~ model + Threshold + Overlap, data = data)

# Base R: write lm diagnostics to an EPS file
setEPS()                                    # ensure EPS not generic PostScript
postscript("./mlr_threemodels_goodcopy_IoU05_additive_diagnostics.eps",            # output filename
           width = 7, height = 7,           # size in inches
           paper = "special", onefile = FALSE)  # EPS-compliant settings

par(mfrow = c(2, 2))        # 2×2 panel layout
plot(model_additive) # draws the 4 diagnostic panels
par(mfrow = c(1, 1))        # restore default
dev.off()                    # **must** close the device
par(mfrow = c(2, 2))        # 2×2 panel layout
plot(model_additive) # draws the 4 diagnostic panels
par(mfrow = c(1, 1))        # restore default

# ----==========----- If we use a strictly simple interaction model ----==========-----
model_interaction <- lm(`IoU.0.50` ~ model + Threshold * Overlap, data = data)

# Base R: write lm diagnostics to an EPS file
setEPS()                                    # ensure EPS not generic PostScript
postscript("./mlr_threemodels_goodcopy_IoU05_interaction_diagnostics.eps",            # output filename
           width = 7, height = 7,           # size in inches
           paper = "special", onefile = FALSE)  # EPS-compliant settings

par(mfrow = c(2, 2))        # 2×2 panel layout
plot(model_interaction) # draws the 4 diagnostic panels
par(mfrow = c(1, 1))        # restore default
dev.off()                    # **must** close the device
par(mfrow = c(2, 2))        # 2×2 panel layout
plot(model_interaction) # draws the 4 diagnostic panels
par(mfrow = c(1, 1))        # restore default

# ----==========----- If we use a strictly simple interaction model ----==========-----
model_interaction_2 <- lm(`IoU.0.50` ~ model * Threshold + Overlap, data = data)

# Base R: write lm diagnostics to an EPS file
setEPS()                                    # ensure EPS not generic PostScript
postscript("./mlr_threemodels_goodcopy_IoU05_interaction_diagnostics.eps",            # output filename
           width = 7, height = 7,           # size in inches
           paper = "special", onefile = FALSE)  # EPS-compliant settings

par(mfrow = c(2, 2))        # 2×2 panel layout
plot(model_interaction_2) # draws the 4 diagnostic panels
par(mfrow = c(1, 1))        # restore default
dev.off()                    # **must** close the device
par(mfrow = c(2, 2))        # 2×2 panel layout
plot(model_interaction_2) # draws the 4 diagnostic panels
par(mfrow = c(1, 1))        # restore default

# ----==========----- If we use a strictly simple interaction model ----==========-----
model_full_interaction <- lm(`IoU.0.50` ~ model * Threshold * Overlap, data = data)

# Base R: write lm diagnostics to an EPS file
setEPS()                                    # ensure EPS not generic PostScript
postscript("./mlr_threemodels_goodcopy_IoU05_interaction_diagnostics.eps",            # output filename
           width = 7, height = 7,           # size in inches
           paper = "special", onefile = FALSE)  # EPS-compliant settings

par(mfrow = c(2, 2))        # 2×2 panel layout
plot(model_full_interaction) # draws the 4 diagnostic panels
par(mfrow = c(1, 1))        # restore default
dev.off()                    # **must** close the device
par(mfrow = c(2, 2))        # 2×2 panel layout
plot(model_full_interaction) # draws the 4 diagnostic panels
par(mfrow = c(1, 1))        # restore default

# ---------------------------- the four residuals vs. fitted plots ----------------------------

# Put your models in a named list for convenience
models <- list(
  'additive: lm(`IoU.0.50` ~ model + Threshold + Overlap)' = model_additive,
  'interaction: lm(`IoU.0.50` ~ model + Threshold * Overlap)' = model_interaction,
  'interaction: lm(`IoU.0.50` ~ model * Threshold + Overlap)' = model_interaction_2,
  'full: lm(`IoU.0.50` ~ model * Threshold * Overlap)' = model_full_interaction
)

# Build one data frame with model label, fitted values and *standardised* residuals
library(broom)     # tidy helpers
library(dplyr)

res_df <- bind_rows(
  lapply(names(models), function(nm) {
    augment(models[[nm]]) |>
      transmute(model = nm,
                fitted = .fitted,
                resid_std = .std.resid)   # .std.resid is what plot.lm uses
  })
)

plot <- ggplot(res_df, aes(fitted, resid_std, colour = model)) +
  geom_point(alpha = 0.6) +                        # the dots
  geom_smooth(se = FALSE, method = "loess", span = 0.8) +  # loess curve = the red line in plot.lm
  geom_hline(yintercept = 0, linetype = 2) +       # reference line
  labs(x = "Fitted values",
       y = "Standardised residuals",
       colour = "Model") +
  theme_minimal() +
  theme(
    legend.text  = element_text(size = 14),   # label size (default ~ 8–9 pt)
    legend.title = element_text(size = 15, face = "bold"),  # title size
    legend.key.size = unit(1.0, "cm"),
    legend.position  = c(0.85, 0.05),     # (x, y) in NPC units (0-1 scale)
    legend.justification = c(1, 0),       # anchor lower-right corner
    legend.background = element_rect(fill = scales::alpha("white", 0.7),
                                     colour = "grey70"),
    legend.key       = element_blank()    # drop key backgrounds
  ) +
  guides(
    colour = guide_legend(override.aes = list(size = 4))   # bump up dot/line size
  )

ggsave("./mlr_threemodels_goodcopy_IoU05_lm_residualsvsfitted.eps",
       plot = plot,
       device = cairo_ps,
       dpi = 300,
       units = 'px')
dev.off()


# ------------ Plot the diagnostic plots for the mAP@0.5 and mAP@0.5:0.95 for the interaction_poly models ------------


setEPS()                                    # ensure EPS not generic PostScript
postscript("./mlr_threemodels_goodcopy_interaction_poly_diagnostics.eps",            # output filename
           width = 14, height = 7,           # size in inches
           paper = "special", onefile = FALSE)  # EPS-compliant settings
## ---- 1. set up the overall plotting area ----
par(
  mfrow = c(2, 4),      # 2 rows × 4 columns of plots
  mar  = c(4, 4, 2, 1), # inner plot margins
  oma  = c(0, 4, 0, 0)  # outer margin on the LEFT for row labels
)

## ---- 2. draw your eight panels (replace with your real code) ----
plot(model_interaction_poly_50)
plot(model_interaction_poly_95)


## ---- 3. add row labels in the outer margin ----
## The at= argument is in *figure* coordinates: 0 = bottom, 1 = top.
mtext("mAP@IoU=0.5", side = 2, line = 2, at = 0.75, outer = TRUE, cex = 1.3, font = 2)
mtext("mAP@IoU=0.5:0.95", side = 2, line = 2, at = 0.25, outer = TRUE, cex = 1.3, font = 2)

dev.off()

# ------------ Plot the prediction plots for the mAP@0.5 and mAP@0.5:0.95 for the interaction_poly models ------------
library(cowplot)
plot_grid(plot_IoU05_predictions, plot_IoU0595_predictions, nrow=1)
ggsave("./mlr_threemodels_goodcopy_interaction_poly_predictions.eps",
       device = cairo_ps,
       scale = 1.3,
       width = 3000,
       height = 1300,
       dpi = 300,
       units = 'px')
dev.off()