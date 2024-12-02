library(modelr)
library(easystats)
library(broom)
library(tidyverse)
library(fitdistrplus)

data("mtcars")
glimpse(mtcars)

# Model 1
mod1 = lm(mpg ~ disp, data = mtcars)
summary(mod1)

ggplot(mtcars, aes(x=disp, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()
 
# Model 2
mod2 = lm(mpg ~ qsec, data = mtcars)
summary(mod2)
ggplot(mtcars, aes(x=disp, y = qsec)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

# Compare
mean(mod1$residuals^2) # Mod1 - Smaller which is better
mean(mod2$residuals^2) # Mod2

# Mod1 is better compared to Mod2 so we will make predictions using Mod1
df <- mtcars %>% # Add this column to df
  add_predictions(mod1)
df %>%  dplyr::select("mpg", "pred")

# Predict dependent values based on hypotheitcal independent values. 
newdf <- data.frame(disp = c(500, 600, 700, 800, 900)) # New dataframe with predictor values we want to assess

# make predictions
pred = predict(mod1, newdata = newdf)

# combine hypothetical input data with hypothetical predictions into one new data frame
hyp_preds <- data.frame(disp = newdf$disp,
                        pred = pred)

# Add new column showing whetehr a data point is real or hypothetical
df$PredictionType <- "Real"
hyp_preds$PredictionType <- "Hypothetical"

# Join real and hypothetical data (with model predictions)
fullpreds <- full_join(df, hyp_preds)

# Plot those predictions on our original graph
ggplot(fullpreds, aes(x=disp, y=pred, color=PredictionType)) +
  geom_point() +
  geom_point(aes(y=mpg), color="Black") +
  theme_minimal()

# Some of these points are negative, which doesn't make any sense because mpg cannot be negative.
# A model that is good at explaining things within the bounds of exisitng data may lose all relevance when you extrapolate out to hypothetical situations.

# Compare predictions from several models simultaneously
# Define a 3rd model
mod3 <- glm(data=mtcars,
            formula = mpg ~ hp + disp + factor(am) + qsec)

# put all models into a list
mods <- list(mod1=mod1, mod2=mod2, mod3=mod3)

# apply "performance" function on all in the list and combine
map(mods, performance) %>%  reduce(full_join)

# gather residuals from all 3 models
mtcars %>% 
  gather_residuals(mod1, mod2, mod3) %>% 
  ggplot(aes(x=model,y=resid,fill=model)) +
  geom_boxplot(alpha=.5) +
  geom_point() +
  theme_minimal()

# gather predictions from all 3 models
mtcars %>% 
  gather_predictions(mod1, mod2, mod3) %>% 
  ggplot(aes(x=disp,y=mpg)) +
  geom_point(size=3) +
  geom_point(aes(y=pred,color=model)) +
  geom_smooth(aes(y=pred, color=model)) +
  theme_minimal() +
  annotate("text",x=250,y=32,label=mod1$call) +
  annotate("text",x=250,y=30,label=mod2$call) +
  annotate("text",x=250,y=28,label=mod3$call)

# Putting the model into interpretable English
report(mod3)





