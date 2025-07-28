# Define the CSV file path
csv_file <- "/Volumes/T7 Shield/data/Moose/ColabOutputs/yolo_by_patchsize_pixels.csv"
# Note that simply the mean_pixel_count doesn't have much bearing on mAP50, since more pixels on average doesn't necessarily help, what helps is seeing the moose up close. Therefore, average bbox size might explain it, but this is what our patching method targets anyway.

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

# How does mean pixel count relate to overlap and size?
model <- lm(`mean_pixel_count_valid` ~ size + overlap, data = data)
summary(model)
par(mfrow = c(2, 2))
plot(model) # Note that there aren't equal numbers of each patch in it, so it isn't a perfect relationship.
par(mfrow = c(1, 1))

# Try out the model with the mean_pixel_count
model_additive <- lm(`mAP50` ~ size + threshold + overlap + Instances + total_pixel_count_valid, data = data)
summary(model_additive)
par(mfrow = c(2, 2))
plot(model_additive)
par(mfrow = c(1, 1))


# producing the outputs
### --- Let's inspect how threshold and overlap (which govern the model training) and patch size (the test) affect mAP ---
model_additive <- lm(`mAP50` ~ size + threshold + overlap + count_images_train, data = data) # a bit too simple
model_interaction <- lm(`mAP50` ~ size * threshold * overlap + count_images_train, data = data) # This setting seems to work decently well, let's do backwards elimination on it.
model_interaction_poly <- lm(`mAP50` ~ size * poly(threshold, 3) * overlap + count_images_train, data = data) # looks good but overfits; needs to use a spline for the suspected nonlinearity
library(splines)
model_spline <- lm(`mAP50` ~ size * ns(threshold, knots = c(0.15)) * overlap + count_images_train, data = data) # spline also


summary(model_additive)
summary(model_interaction)
summary(model_interaction_poly)
summary(model_spline)
par(mfrow = c(2, 2))
plot(model_spline)
par(mfrow = c(1, 1))





# produce a plot that shows why model_spline_2 is better than model_interaction_poly 
# generate the diagnostic plots
setEPS()                                    # ensure EPS not generic PostScript
postscript("/Volumes/T7 Shield/data/Moose/ColabOutputs/mlr_threemodels_goodcopy/yolo_by_patchsize_interaction_poly_spline_diagnostics.eps",            # output filename
           width = 14, height = 7,           # size in inches
           paper = "special", onefile = FALSE)  # EPS-compliant settings
## ---- 1. set up the overall plotting area ----
par(
  mfrow = c(2, 4),      # 2 rows × 4 columns of plots
  mar  = c(4, 4, 2, 1), # inner plot margins
  oma  = c(0, 4, 0, 0)  # outer margin on the LEFT for row labels
)

## Models
library(splines)


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
postscript("/Volumes/T7 Shield/data/Moose/ColabOutputs/mlr_threemodels_goodcopy/yolo_by_patchsize_residuals_vs_fitted.eps",            # output filename
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
mtext("no Images", side = 2, line = 2, at = 0.75, outer = TRUE, cex = 1.3, font = 2)
mtext("+ Images", side = 2, line = 2, at = 0.25, outer = TRUE, cex = 1.3, font = 2)

mtext("additive", side = 3, line = 1, outer=TRUE, at = 0.14, cex = 1.3, font = 2)
mtext("interaction", side = 3, line = 1, outer=TRUE, at = 0.39, cex = 1.3, font = 2)
mtext("poly", side = 3, line = 1, outer=TRUE, at = 0.64, cex = 1.3, font = 2)
mtext("spline", side = 3, line = 1, outer=TRUE, at = 0.89, cex = 1.3, font = 2)

dev.off()





# redoing the analysis with the model_additive, model_interaction, model_interaction_poly, model_spline using count_images_train:

library(splines)
model_additive <- lm(`mAP50` ~ size + threshold + overlap + count_images_train, data = data) # a bit too simple
model_interaction <- lm(`mAP50` ~ size * threshold * overlap + count_images_train, data = data) # This setting seems to work decently well, let's do backwards elimination on it.
model_interaction_poly <- lm(`mAP50` ~ size * poly(threshold, 3) * overlap + count_images_train, data = data) # looks good but overfits; needs to use a spline for the suspected nonlinearity
library(splines)
model_spline <- lm(`mAP50` ~ size * ns(threshold, knots = c(0.15)) * overlap + count_images_train, data = data) # spline also



# Let's try the analysis again, with the new model.
library(caret)
set.seed(2025)
summary(model_additive)
summary(model_interaction)
summary(model_interaction_poly)
summary(model_spline)
anova(model_additive, model_interaction, model_interaction_poly, model_spline) # anova

# cross-validated
ctrl <- trainControl(method="cv", number=10)

model_additive_cv <- train(`mAP50` ~ size + threshold + overlap + count_images_train, data=data, method="lm", trControl=ctrl, metric="RMSE")
model_interaction_cv <- train(`mAP50` ~ size * threshold * overlap + count_images_train, data=data, method="lm", trControl=ctrl, metric="RMSE")
model_interaction_poly_cv <- train(`mAP50` ~ size * poly(threshold, 3) * overlap + count_images_train, data=data, method="lm", trControl=ctrl, metric="RMSE")
model_spline_cv <- train(`mAP50` ~ size * ns(threshold, knots = c(0.15)) * overlap + count_images_train, data=data, method="lm", trControl=ctrl, metric="RMSE")

c(model_additive_RMSE = model_additive_cv$results$RMSE,
  model_interaction_cv_RMSE   = model_interaction_cv$results$RMSE,
  model_interaction_poly_cv_RMSE  = model_interaction_poly_cv$results$RMSE,
  model_spline_RMSE = model_spline_cv$results$RMSE)

# info criteria
AIC(model_additive, model_interaction, model_interaction_poly, model_spline)
BIC(model_additive, model_interaction, model_interaction_poly, model_spline)



