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

library(splines)
model_additive <- lm(`IoU.0.50` ~ model + Threshold + Overlap, data = data)
model_additive_poly <- lm(`IoU.0.50` ~ model + poly(Threshold, 3) + Overlap, data = data)
model_interaction <- lm(`IoU.0.50` ~ model + Threshold * Overlap, data = data)
model_interaction_full <- lm(`IoU.0.50` ~ model * Threshold * Overlap, data = data)
model_interaction_poly <- lm(`IoU.0.50` ~ model + poly(Threshold, 3) * Overlap, data = data)
model_spline <- lm(`IoU.0.50` ~ model + ns(Threshold, knots = c(0.15)) * Overlap, data = data)
model_spline_full <- lm(`IoU.0.50` ~ model * ns(Threshold, knots = c(0.15)) * Overlap, data = data)



# Analysis
library(caret)
set.seed(2025)
summary(model_additive)
summary(model_additive_poly)
summary(model_interaction)
summary(model_interaction_full)
summary(model_interaction_poly)
summary(model_spline)
summary(model_spline_full)
anova(model_additive, model_additive_poly, model_interaction, model_interaction_full, model_interaction_poly, model_spline, model_spline_full) # anova

# cross-validated
ctrl <- trainControl(method="cv", number=10)

model_additive_cv <- train(`IoU.0.50` ~ model + Threshold + Overlap, data=data, method="lm", trControl=ctrl, metric="RMSE")
model_additive_poly_cv <- train(`IoU.0.50` ~ model + poly(Threshold, 3) + Overlap, data=data, method="lm", trControl=ctrl, metric="RMSE")
model_interaction_cv <- train(`IoU.0.50` ~ model + Threshold * Overlap, data=data, method="lm", trControl=ctrl, metric="RMSE")
model_interaction_full_cv <- train(`IoU.0.50` ~ model * Threshold * Overlap, data=data, method="lm", trControl=ctrl, metric="RMSE")
model_interaction_poly_cv <- train(`IoU.0.50` ~ model + poly(Threshold, 3) * Overlap, data=data, method="lm", trControl=ctrl, metric="RMSE")
model_spline_cv <- train(`IoU.0.50` ~ model + ns(Threshold, knots = c(0.15)) * Overlap, data=data, method="lm", trControl=ctrl, metric="RMSE")
model_spline_full_cv <- train(`IoU.0.50` ~ model * ns(Threshold, knots = c(0.15)) * Overlap, data=data, method="lm", trControl=ctrl, metric="RMSE")

c(model_additive_RMSE = model_additive_cv$results$RMSE,
  model_additive_poly_cv_RMSE = model_additive_poly_cv$results$RMSE,
  model_interaction_cv_RMSE   = model_interaction_cv$results$RMSE,
  model_interaction_full_cv_RMSE   = model_interaction_full_cv$results$RMSE,
  model_interaction_poly_cv_RMSE  = model_interaction_poly_cv$results$RMSE,
  model_spline_RMSE = model_spline_cv$results$RMSE,
  model_spline_full_RMSE = model_spline_full_cv$results$RMSE)

# info criteria
AIC(model_additive, model_additive_poly, model_interaction, model_interaction_full, model_interaction_poly, model_spline, model_spline_full)
BIC(model_additive, model_additive_poly, model_interaction, model_interaction_full, model_interaction_poly, model_spline, model_spline_full)


par(mfrow = c(2, 2))
plot(model_interaction_poly)
par(mfrow = c(1, 1))



# --- backwards elimination; can we do a full interaction model? ---
# full interaction model (what you already have)
th = poly(data$Threshold, 3)
model_full <- lm(`IoU.0.50` ~ model * th * Overlap, data = data)

# Backward elimination by AIC
model_back <- step(
  model_full,
  direction = "backward",   # only remove terms
  trace      = TRUE         # print each step
)
summary(model_back)

library(MASS)
model_back_aic <- stepAIC(
  model_full,
  direction = "backward",
  trace = FALSE   # quieter output
)
summary(model_back_aic)

library(olsrr)
model_back_ols <- ols_step_backward_aic(model_full)
summary(model_back_ols)

library(caret)
ctrl <- rfeControl(functions = lmFuncs, 
                   method = "cv", 
                   number = 10,
                   rerank = FALSE,
                   allowParallel = FALSE)
set.seed(2025)
dummies <- dummyVars(~ size, data = data)      # one-hot encode
X       <- as.data.frame(predict(dummies, data))
X$threshold <- data$threshold
X$overlap <- data$overlap
X$Images    <- data$Images
X$Instances <- data$Instances

model_rfe <- rfe(x = X,
                 y = data$mAP50,
                 sizes = 1:7,
                 rfeControl = ctrl)

model_rfe


# Plot
library(ggplot2)
library(ggeffects)       # install.packages("ggeffects")
library(cowplot)

# with discrete colour maps
library(scales)  # for rescale()
library(grid)   # for unit()

pred_df <- ggpredict(model_interaction_poly, terms = c("Threshold [all]", "Overlap"))

# numeric version of overlap (e.g. 0, 0.1, 0.3)
data$Overlap_num  <- as.numeric(as.character(data$Overlap))
pred_df$group_num <- as.numeric(as.character(pred_df$group))

plt1 <- ggplot(pred_df, aes(x = x, y = predicted, colour = group_num)) +
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
  labs(x = "Threshold", y = "Predicted mAP@0.50") +
  theme_minimal()

plt1 <- ggplot(pred_df, aes(x = x, y = predicted, colour = group_num)) +
  geom_line(aes(group = group_num), size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high,
                  fill = group_num,  group = group_num),
              alpha = 0.20, colour = NA) +
  geom_point(data = data,
             aes(x = Threshold, y = IoU.0.50, colour = Overlap_num),
             position = position_jitter(width = 0.01, height = 0),
             size = 1.5, alpha = 0.8) +
  scale_colour_viridis_c(
    name    = "Overlap",
    option  = "D",
    limits  = c(0, 0.3),          # scale runs from 0 to 0.3
    breaks  = c(0.0, 0.1, 0.3),   # ticks you want
    labels  = scales::number_format(accuracy = 0.1)  # 0.0, 0.1, 0.3
  ) +
  scale_fill_viridis_c(
    name    = "Overlap",
    option  = "D",
    limits  = c(0, 0.3),
    breaks  = c(0.0, 0.1, 0.3),
    labels  = scales::number_format(accuracy = 0.1)
  ) +
  labs(x = "Threshold", y = "Predicted mAP@0.50") +
  theme_minimal()

# Colour stops (duplicate each colour at its boundary)
cols   <- c("red","red", "blue","blue", "green","green")
values <- rescale(c(0, 0.05, 0.05, 0.15, 0.15, 0.3))  # positions must be 0–1

plt1 = plt1 +
  scale_colour_gradientn(
    name   = "Overlap",
    colours = cols,
    values  = values,
    limits  = c(0, 0.3),        # full range of Overlap
    breaks  = c(0, 0.1, 0.3),
    labels  = c("0","0.1","0.3")
  ) +
  scale_fill_gradientn(
    name   = "Overlap",
    colours = cols,
    values  = values,
    limits  = c(0, 0.3),
    breaks  = c(0, 0.1, 0.3),
    labels  = c("0","0.1","0.3")
  )

plt1 = plt1 +
  ## 1⃣  move the whole legend a little lower
  #theme(
  #  legend.position      = c(.88, .20),      # (x, y) in [0,1] NPC
  #  legend.justification = c(1, 0)           # anchor = bottom-right corner
  #) +
  
  ## 2⃣  colour the tick marks on the colour bar black
  guides(
    colour = guide_colourbar(
      ticks        = TRUE,                   # keep the ticks
      ticks.colour = "black",
      ticks.linewidth = 1                    # optional – make them bolder
    ),
    fill   = guide_colourbar(
      ticks        = TRUE,
      ticks.colour = "black",
      ticks.linewidth = 1
    )
  )


pred_df <- ggpredict(model_interaction_poly_95, terms = c("Threshold [all]", "Overlap"))

# numeric version of overlap (e.g. 0, 0.1, 0.3)
data$Overlap_num  <- as.numeric(as.character(data$Overlap))
pred_df$group_num <- as.numeric(as.character(pred_df$group))

plt2 <- ggplot(pred_df, aes(x = x, y = predicted, colour = group_num)) +
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
  labs(x = "Threshold", y = "Predicted mAP@0.50") +
  theme_minimal()

plt2 <- ggplot(pred_df, aes(x = x, y = predicted, colour = group_num)) +
  geom_line(aes(group = group_num), size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high,
                  fill = group_num,  group = group_num),
              alpha = 0.20, colour = NA) +
  geom_point(data = data,
             aes(x = Threshold, y = IoU.0.50.0.95, colour = Overlap_num),
             position = position_jitter(width = 0.01, height = 0),
             size = 1.5, alpha = 0.8) +
  scale_colour_viridis_c(
    name    = "Overlap",
    option  = "D",
    limits  = c(0, 0.3),          # scale runs from 0 to 0.3
    breaks  = c(0.0, 0.1, 0.3),   # ticks you want
    labels  = scales::number_format(accuracy = 0.1)  # 0.0, 0.1, 0.3
  ) +
  scale_fill_viridis_c(
    name    = "Overlap",
    option  = "D",
    limits  = c(0, 0.3),
    breaks  = c(0.0, 0.1, 0.3),
    labels  = scales::number_format(accuracy = 0.1)
  ) +
  labs(x = "Threshold", y = "Predicted mAP@0.50.0.95") +
  theme_minimal()


# Colour stops (duplicate each colour at its boundary)
cols   <- c("red","red", "blue","blue", "green","green")
values <- rescale(c(0, 0.05, 0.05, 0.15, 0.15, 0.3))  # positions must be 0–1

plt2 = plt2 +
  scale_colour_gradientn(
    name   = "Overlap",
    colours = cols,
    values  = values,
    limits  = c(0, 0.3),        # full range of Overlap
    breaks  = c(0, 0.1, 0.3),
    labels  = c("0","0.1","0.3")
  ) +
  scale_fill_gradientn(
    name   = "Overlap",
    colours = cols,
    values  = values,
    limits  = c(0, 0.3),
    breaks  = c(0, 0.1, 0.3),
    labels  = c("0","0.1","0.3")
  )

plt2 = plt2 +
  ## 1⃣  move the whole legend a little lower
  #theme(
  #  legend.position      = c(.88, .20),      # (x, y) in [0,1] NPC
  #  legend.justification = c(1, 0)           # anchor = bottom-right corner
  #) +
  
  ## 2⃣  colour the tick marks on the colour bar black
  guides(
    colour = guide_colourbar(
      ticks        = TRUE,                   # keep the ticks
      ticks.colour = "black",
      ticks.linewidth = 1                    # optional – make them bolder
    ),
    fill   = guide_colourbar(
      ticks        = TRUE,
      ticks.colour = "black",
      ticks.linewidth = 1
    )
  )


library(cowplot)
plot_grid(plt1, plt2, nrow=1)
ggsave("./mlr_threemodels_goodcopy_interaction_poly_predictions_2.eps",
       device = cairo_ps,
       scale = 1.3,
       width = 3000,
       height = 1300,
       dpi = 300,
       units = 'px')
dev.off()