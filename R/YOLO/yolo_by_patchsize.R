# Define the CSV file path
csv_file <- "./yolo_by_patchsize_pixels.csv"

# Read the CSV file
data <- read.csv(csv_file)

# Convert 'model' to a factor (categorical variable)
data$size <- as.factor(data$size)

### --- Let's inspect how patch size affects mAP50 (accuracy) performance ---

model <- lm(`mAP50` ~ size, data = data)

# Display a summary of the regression results
summary(model)


# by row
data_small = data[data$size == "small", ]
data_medium = data[data$size == "medium", ]
data_large = data[data$size == "large", ]
data_small
data_medium
data_large

# Average mAP by patch size
data_small_mean = mean(data_small$`mAP50`, na.rm = TRUE)
data_medium_mean = mean(data_medium$`mAP50`, na.rm = TRUE)
data_large_mean = mean(data_large$`mAP50`, na.rm = TRUE)
data_small_mean # 0.918
data_medium_mean # 0.883
data_large_mean # 0.838

# takeaways:
# When evaluating the trained models (trained on all image sizes), they tested best on the small images. This makes sense, since they have more details.
# For small images, the effect is highly significant.


# producing the outputs
### --- Let's inspect how threshold and overlap (which govern the model training) and patch size (the test) affect mAP ---
model_additive <- lm(`mAP50` ~ size + threshold + overlap + count_images_train, data = data) # a bit too simple
model_interaction_poly <- lm(`mAP50` ~ size * poly(threshold, 3) * overlap + count_images_train, data = data) # looks good but overfits; needs to use a spline for the suspected nonlinearity
model_interaction <- lm(`mAP50` ~ size * threshold * overlap + count_images_train, data = data) # This setting seems to work decently well, let's do backwards elimination on it.
library(splines)
model_spline_full <- lm(`mAP50` ~ size * ns(threshold, knots = c(0.15)) * overlap + count_images_train, data = data) # spline also
model_spline_2 <- lm(mAP50 ~ size * ns(threshold, knots = c(0.15)) * overlap + count_images_train, data = data)
par(mfrow = c(2, 2))
plot(model_interaction_poly)
par(mfrow = c(1, 1))


# produce a plot that shows why model_spline_2 is better than model_interaction_poly 
# generate the diagnostic plots
setEPS()                                    # ensure EPS not generic PostScript
postscript("./mlr_threemodels_goodcopy/yolo_by_patchsize_interaction_poly_spline_diagnostics.eps",            # output filename
           width = 14, height = 7,           # size in inches
           paper = "special", onefile = FALSE)  # EPS-compliant settings
## ---- 1. set up the overall plotting area ----
par(
  mfrow = c(2, 4),      # 2 rows × 4 columns of plots
  mar  = c(4, 4, 2, 1), # inner plot margins
  oma  = c(0, 4, 0, 0)  # outer margin on the LEFT for row labels
)


## ---- 2. draw your eight panels (replace with your real code) ----
plot(model_interaction_poly, sub.caption = "")
plot(model_spline_2, sub.caption = "")


## ---- 3. add row labels in the outer margin ----
## The at= argument is in *figure* coordinates: 0 = bottom, 1 = top.
mtext("model_interaction_poly", side = 2, line = 2, at = 0.75, outer = TRUE, cex = 1.3, font = 2)
mtext("model_spline", side = 2, line = 2, at = 0.25, outer = TRUE, cex = 1.3, font = 2)

dev.off()



# generate the diagnostic plots
setEPS()                                    # ensure EPS not generic PostScript
postscript("./mlr_threemodels_goodcopy/yolo_by_patchsize_residuals_vs_fitted.eps",            # output filename
           width = 14, height = 7,           # size in inches
           paper = "special", onefile = FALSE)  # EPS-compliant settings
## ---- 1. set up the overall plotting area ----
par(
  mfrow = c(2, 4),      # 2 rows × 4 columns of plots
  mar  = c(4, 4, 2, 1), # inner plot margins
  oma  = c(0, 4, 4, 0)  # outer margin on the LEFT for row labels
)

## Models
library(splines)
a <- lm(`mAP50` ~ size + threshold + overlap, data = data) # a bit too simple
i <- lm(`mAP50` ~ size * threshold * overlap, data = data) # This setting seems to work decently well, let's do backwards elimination on it.
ip <- lm(`mAP50` ~ size * poly(threshold, 3) * overlap, data = data) # looks good but overfits; needs to use a spline for the suspected nonlinearity
s <- lm(`mAP50` ~ size * ns(threshold, knots = c(0.15)) * overlap, data = data) # spline also

a_i <- lm(`mAP50` ~ size + threshold + overlap + count_images_train, data = data) # a bit too simple
i_i <- lm(`mAP50` ~ size * threshold * overlap + count_images_train, data = data) # This setting seems to work decently well, let's do backwards elimination on it.
ip_i <- lm(`mAP50` ~ size * poly(threshold, 3) * overlap + count_images_train, data = data) # looks good but overfits; needs to use a spline for the suspected nonlinearity
s_i <- lm(`mAP50` ~ size * ns(threshold, knots = c(0.15)) * overlap + count_images_train, data = data) # spline also

## ---- 2. draw your eight panels (replace with your real code) ----
plot(a, which=1, sub.caption = "")
plot(i, which=1, sub.caption = "")
plot(ip, which=1, sub.caption = "")
plot(s, which=1, sub.caption = "")
plot(a_i, which=1, sub.caption = "")
plot(i_i, which=1, sub.caption = "")
plot(ip_i, which=1, sub.caption = "")
plot(s_i, which=1, sub.caption = "")


## ---- 3. add row labels in the outer margin ----
## The at= argument is in *figure* coordinates: 0 = bottom, 1 = top.
mtext("no count_images_train", side = 2, line = 2, at = 0.75, outer = TRUE, cex = 1.3, font = 2)
mtext("+ count_images_train", side = 2, line = 2, at = 0.25, outer = TRUE, cex = 1.3, font = 2)

mtext("additive", side = 3, line = 1, outer=TRUE, at = 0.14, cex = 1.3, font = 2)
mtext("interaction", side = 3, line = 1, outer=TRUE, at = 0.39, cex = 1.3, font = 2)
mtext("poly", side = 3, line = 1, outer=TRUE, at = 0.64, cex = 1.3, font = 2)
mtext("spline", side = 3, line = 1, outer=TRUE, at = 0.89, cex = 1.3, font = 2)

dev.off()







### --- Let's inspect how threshold and overlap (which govern the model training) and patch size (the test) affect mAP ---
model_additive <- lm(`mAP50` ~ size + threshold + overlap + count_images_train + count_annotations_train, data = data) # a bit too simple
model_interaction_poly <- lm(`mAP50` ~ size * poly(threshold, 3) * overlap + count_images_train + count_annotations_train, data = data) # looks good but overfits; needs to use a spline for the suspected nonlinearity
model_interaction <- lm(`mAP50` ~ size * threshold * overlap + count_images_train + count_annotations_train, data = data) # This setting seems to work decently well, let's do backwards elimination on it.
library(splines)
model_spline_full <- lm(`mAP50` ~ size * ns(threshold, knots = c(0.15)) * overlap + count_images_train + count_annotations_train, data = data) # spline also
par(mfrow = c(2, 2))
plot(model_interaction)
par(mfrow = c(1, 1))

# --- backwards elimination ---
# full interaction model (what you already have)
model_full <- model_interaction

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
X$count_images_train    <- data$count_images_train
X$count_annotations_train <- data$count_annotations_train

model_rfe <- rfe(x = X,
                 y = data$mAP50,
                 sizes = 1:7,
                 rfeControl = ctrl)

model_rfe # overlap, size.large, size.medium, threshold, count_images_train. So, count_images_train are more important than count_annotations_train.



# --- backwards elimination for spline version ---
model_full <- model_interaction_poly
threshold = data$threshold

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
X$count_images_train    <- data$count_images_train
X$count_annotations_train <- data$count_annotations_train

model_rfe <- rfe(x = X,
                 y = data$mAP50,
                 sizes = 1:7,
                 rfeControl = ctrl)
model_rfe # overlap, size.large, size.medium, threshold, count_images_train. So, count_images_train are more important than count_annotations_train. So, it works for these as well!




ctrl <- rfeControl(functions = caretFuncs,
                   method    = "cv",
                   number    = 10,
                   rerank    = FALSE,
                   allowParallel = FALSE)
model_rfe <- rfe(x = data[, c("size","threshold","overlap","count_images_train","count_annotations_train")],
                 y = data$mAP50,
                 sizes = 1:7,
                 rfeControl = ctrl)
model_rfe # threshold, size, count_images_train, count_annotations_train; this is the same as the previous experiment.

nzv <- nearZeroVar(data[, c("size","threshold","overlap","count_images_train","count_annotations_train")],
                   saveMetrics = TRUE)
nzv # there are no non-zero variances, so we are safe (we didn't see these get removed anyway).

print(model_rfe)
# Recursive feature selection
# ...
plot(model_rfe, type = c("o","g"))   # elbow plot of RMSE vs #predictors


model_interaction_old <- lm(`mAP50` ~ size * threshold * overlap + count_images_train + count_annotations_train, data = data)
model_interaction <- lm(`mAP50` ~ size * threshold * overlap + count_images_train, data = data)
anova(model_interaction_old, model_interaction) # Pr(>F) = 0.1269, so again, we can use the simpler version.

model_interaction_poly <- lm(`mAP50` ~ size * poly(threshold, 3) * overlap + count_images_train, data = data)
summary(model_interaction) # no errors anymore.

summary(model_interaction_poly)
# Plot diagnostic plots to check assumptions
par(mfrow = c(2, 2))
plot(model_interaction_poly)
par(mfrow = c(1, 1))

# 'count_images_train' (image count) does not seem to have much impact at all, with a super-high p value and a tiny estimate.
# model_interaction <- lm(`mAP50` ~ size * poly(threshold, 3) * overlap + count_images_train, data = data) produces a good result except for 3,6,9 (threshold = 0.1 and size = small)
# Image size, which we have here, is naturally going to be important (and correlated with overlap).

# which of model_interaction and model_interaction_poly should I use?

library(caret)
anova(model_interaction, model_interaction_poly) # anova

# cross-validated
ctrl <- trainControl(method="cv", number=10)

m_lin_cv <- train(mAP50 ~ size * threshold * overlap + count_images_train,
                  data=data, method="lm", trControl=ctrl, metric="RMSE")
m_poly_cv <- train(mAP50 ~ size * poly(threshold,3) * overlap + count_images_train,
                   data=data, method="lm", trControl=ctrl, metric="RMSE")

c(linear_RMSE = m_lin_cv$results$RMSE,
  poly_RMSE   = m_poly_cv$results$RMSE)

# info criteria
AIC(model_interaction, model_interaction_poly)
BIC(model_interaction, model_interaction_poly)



# try again with modded data; we get the same kinds of results
data$th_c <- scale(data$threshold, center=TRUE, scale=FALSE)
model_interaction <- lm(`mAP50` ~ size * th_c * overlap + count_images_train, data = data)
model_interaction_poly <- lm(`mAP50` ~ size * poly(th_c, 3) * overlap + count_images_train, data = data)
anova(model_interaction, model_interaction_poly) # anova

# cross-validated
ctrl <- trainControl(method="cv", number=10)

m_lin_cv <- train(mAP50 ~ size * th_c * overlap + count_images_train,
                  data=data, method="lm", trControl=ctrl, metric="RMSE")
m_poly_cv <- train(mAP50 ~ size * poly(th_c,3) * overlap + count_images_train,
                   data=data, method="lm", trControl=ctrl, metric="RMSE")

c(linear_RMSE = m_lin_cv$results$RMSE,
  poly_RMSE   = m_poly_cv$results$RMSE)

# info criteria
AIC(model_interaction, model_interaction_poly)
BIC(model_interaction, model_interaction_poly)




# !!! verdict: we use the non-polynomial interaction model !!!
model_interaction <- lm(`mAP50` ~ size * threshold * overlap + count_images_train, data = data) # <-- this one
model_interaction_poly <- lm(`mAP50` ~ size * poly(threshold, 3) * overlap + count_images_train, data = data)
# Plot diagnostic plots to check assumptions
par(mfrow = c(2, 2))
plot(model_interaction_poly)
par(mfrow = c(1, 1))


# what do we do, considering that points #3, #6, #9 have excess leverage?

library(splines)         # natural spline with knot at 0.15
data$th_c <- with(data, threshold - 0.15)          # center for stability; Put a knot at threshold ≈ 0.15 so the curve can bend sharply where you see trouble, but keep interactions parsimonious
model_spline <- lm(mAP50 ~ size * ns(th_c, knots = 0) * overlap + count_images_train,
                   data = data)
summary(model_spline)

model_spline_2 <- lm(mAP50 ~ size * ns(threshold, knots = c(0.15)) * overlap + count_images_train,
                   data = data)
summary(model_spline_2)

par(mfrow = c(2, 2))
plot(model_interaction_poly)
par(mfrow = c(1, 1))


par(mfrow = c(2, 2))
plot(model_spline_2)
par(mfrow = c(1, 1))


library(effects)
plot(predictorEffects(model_spline_2, ~ threshold | size))


# Let's try the analysis again, with the new model.
library(caret)
set.seed(2025)
model_additive
model_interaction
model_interaction_poly
model_spline_2
anova(model_additive, model_interaction, model_interaction_poly, model_spline_2) # anova

# cross-validated
ctrl <- trainControl(method="cv", number=10)

m_add_cv <- train(mAP50 ~ size + threshold + overlap + count_images_train,
                  data=data, method="lm", trControl=ctrl, metric="RMSE")
m_lin_cv <- train(mAP50 ~ size * threshold * overlap + count_images_train,
                  data=data, method="lm", trControl=ctrl, metric="RMSE")
m_poly_cv <- train(mAP50 ~ size * poly(threshold,3) * overlap + count_images_train,
                   data=data, method="lm", trControl=ctrl, metric="RMSE")
m_spline_2_cv <- train(mAP50 ~ size * ns(threshold, knots = c(0.15)) * overlap + count_images_train, 
                     data=data, method="lm", trControl=ctrl, metric="RMSE")

c(additive_RMSE = m_add_cv$results$RMSE,
  linear_RMSE = m_lin_cv$results$RMSE,
  poly_RMSE   = m_poly_cv$results$RMSE,
  spline_RMSE = m_spline_2_cv$results$RMSE)

# info criteria
AIC(model_additive, model_interaction, model_interaction_poly, model_spline_2)
BIC(model_additive, model_interaction, model_interaction_poly, model_spline_2)

# Can we plot anything?

library(ggplot2)
library(ggeffects)       # install.packages("ggeffects")

# Get predictions for each Overlap × Threshold combination
model_spline_2 <- lm(mAP50 ~ size * ns(threshold, knots = c(0.15)) * overlap + count_images_train,
                     data = data)
pred_df <- ggpredict(model_spline_2, terms = c("threshold [all]", "overlap"))

# numeric version of overlap (e.g. 0, 0.1, 0.3)
data$overlap_num  <- as.numeric(as.character(data$overlap))
pred_df$group_num <- as.numeric(as.character(pred_df$group))

plot_IoU05_predictions <- ggplot(pred_df, aes(x = x, y = predicted, colour = group_num)) +
  scale_colour_viridis_c(name = "Overlap", option = "D") +
  geom_line(aes(group = group_num), size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high,
                  fill = group_num,  group = group_num),   # <- added group
              alpha = 0.20, colour = NA) +
  scale_fill_viridis_c(name = "Overlap", option = "D") +
  geom_point(data = data,
             aes(x = threshold, y = mAP50, colour = overlap_num),
             position = position_jitter(width = 0.01, height = 0),
             size = 1.5, alpha = 0.8) +
  labs(x = "Threshold", y = "Predicted mAP@0.5") +
  theme_minimal()

print(plot_IoU05_predictions)




# Plot
library(effects)   # builds partial-effect grids
library(ggplot2)

library(splines)
kn <- 0.15
ns_mat <- ns(data$threshold, knots = kn)
colnames(ns_mat) <- paste0("ns", seq_len(ncol(ns_mat)))  # ns1, ns2, ns3

matplot(data$threshold, ns_mat,
        pch = 19, col = 1:3, type = "p",
        xlab = "Threshold", ylab = "Basis function value")
legend("topright", legend = colnames(ns_mat), pch = 19, col = 1:3)
abline(v = kn, lty = 2)     # show knot location





model_spline_factor <- lm(mAP50 ~ size * ns(threshold, knots = c(0.15)) * as.factor(overlap) + count_images_train, data = data)
pred_grid <- expand.grid(
  threshold = seq(min(data$threshold), max(data$threshold), length = 100),
  size      = levels(data$size),
  overlap   = levels(as.factor(data$overlap)),
  count_images_train    = median(data$count_images_train)      # hold numeric covariates constant
)

pred_grid$mAP50 <- predict(model_spline_factor, newdata = pred_grid)

ggplot(pred_grid,
       aes(threshold, mAP50, colour = size)) +
  geom_line(size = 1) +
  facet_grid(overlap ~ size) +      # row = overlap, col = size
  theme_minimal() +
  labs(x = "Threshold",
       y = "Predicted mAP50",
       colour = "Image size")







# Cook's distance; only 1,2 and 7 exceed the 4/36 = 0.11 rule of thumb
cooks.distance(model_additive)

# --- Since the same model is used across threshold and overlap rows, we can use lmer ---
library(tidyverse)
library(lme4)
library(lmerTest)   # p-values for lmer
library(emmeans)    # marginal means & contrasts
library(dplyr)
library(regclass)
library(car)

data <- read.csv("yolo_by_patchsize.csv") |>
  mutate(ModelID = interaction(threshold, overlap, drop = TRUE))

table(data$ModelID)

# 1. Fit additive and full-interaction models
m_add  <- lmer(mAP50 ~ size + threshold + overlap + (1|ModelID), data = data)
m_full <- lmer(mAP50 ~ size * threshold * overlap + (1|ModelID), data = data)

# 2. Compare
anova(m_add, m_full)          # LRT ∴ does the interaction improve fit?
# There is systematic signal in the higher terms
summary(m_full)               # see which terms survive
# Quick mental picture
# Large patches love higher thresholds (threshold is significant and quite large at 0.17778); small patches still benefit but not as steeply, and benefit less when overlap is low (sizesmall:threshold is negative and significant at -0.10303). With large patches, small partial boxes are extremely hard to detect. When patches are at their smallest (size small and no overlap), we relieve this reliance on making it easier using thresholding.
# sizesmall × overlap –0.182 0.064 (.) overlap hurts small patches more than large (weak-sig.); again, perhaps because the benefit is from the smallness.
vif(lm(mAP50 ~ threshold*overlap*size, data=data))  # quick collinearity check
vif(lm(mAP50 ~ threshold*overlap*size, data=data), type='predictor')  # quick collinearity check

# 3. Marginal means for each PatchSize at representative Threshold/Overlap
emm <- emmeans(m_full, ~ size | threshold + overlap,
               at=list(threshold=c(.1,.3,.5), overlap=c(.0,.1,.3)))
plot(emm)                     # interaction slice plots

# More analysis
library(lme4); library(lmerTest)
library(performance); library(DHARMa); library(influence.ME)
library(lattice)

## 1. Basic mixed-model plots
plot(m_full)                       # residuals vs fitted etc.
qqmath(ranef(m_full, condVar = TRUE)) # random-effect QQ

## 2. One-click dashboard
check_model(m_full)                # 5 diagnostic panels

## 3. Simulation-based residuals
sim <- simulateResiduals(fittedModel = m_full, n = 500)
plot(sim)                          # uniform quantile residuals
testResiduals(sim)                 # KS & dispersion tests

## 4. Influence by checkpoint
infl <- influence(m_full, group = "ModelID")
plot(infl, which = "cook")


# --- Let's analyze the effect that overlap and size have on the inference speed ---

library(tidyverse); library(lme4); library(emmeans)
data <- read_csv("yolo_by_patchsize.csv") |>
  mutate(
    ModelID = interaction(threshold, overlap, drop = TRUE),
    overlap = factor(overlap, levels = c(0.0, 0.1, 0.3)),
    time_ms = 1000 / speedinference,
    log_time = log10(time_ms)
  )

m_full <- lmer(log_time ~ size * overlap + (1 | ModelID), data = data)

plot(m_full)                         # residuals vs fitted
qqnorm(resid(m_full)); qqline(resid(m_full))
car::vif(m_full)

anova(m_full, type = "III")
emm  <- emmeans(m_full, ~ size | overlap)
pairs(emm, adjust = "tukey")
summary(emm, type = "response")   # back-transformed ms

# As expected from the fact that YOLO resizes all images, there is no effect beyond random GPU jitter noise.