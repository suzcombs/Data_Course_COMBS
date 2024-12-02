#### Assignment 8 ####
## Load packages
library(modelr)
library(GGally)
library(easystats)
library(broom)
library(fitdistrplus)
library(tidyverse)

### 1. Load the data set ####
df <- read_csv("../../Data/mushroom_growth.csv")
glimpse(df)

# Change species and humidity to factors
df$Species <- as.factor(df$Species)
df$Humidity <- as.factor(df$Humidity)

### 2. Create several plots exploring relationships between the response and predictors ####
# Look at different possibilities all at once using ggplairs
ggpairs(df) 

# GrowthRate is right-skewed. Adding a transformation for a normal distribution
df$log_GrowthRate <- log(df$GrowthRate)
ggplot(df, aes(x=log_GrowthRate)) + # Checking it out and it looks much more normal so will probably plot better
  geom_density()

## Plots ####
## Lights effect on GrowthRate by Species
# Looks the more light, the higher the growth rate especially for P.cornucopiae
ggplot(df, aes(x = factor(Light), y = log_GrowthRate, fill = Species)) +
  geom_boxplot()

ggplot(df, aes(x = Light, y = log_GrowthRate, color = Species)) +
  geom_point()

## Nitrogen's effect on GrowthRate by Species
# Looks like 20 is the ideal amount for Nitrogen in Growthrate
ggplot(df, aes(x = factor(Nitrogen), y = log_GrowthRate, fill = Species)) +
  geom_boxplot()

ggplot(df, aes(x = Nitrogen, y = log_GrowthRate, color = Species)) +
  geom_smooth(se=FALSE)

## Humidity's effect on GrowthRate by Species
# Higher humidity seems to improve growth rate especially for P.cornucopiae species
ggplot(df, aes(x = Humidity, y = log_GrowthRate, fill = Species)) +
  geom_boxplot()

ggplot(df, aes(x = Humidity, y = log_GrowthRate, color = Species)) +
  geom_point()

## Temperature's effect on GrowthRate by Speceis
# Doesn't seems to be significant, though P.cornucopiae seems to have somewhat more growth with 20
ggplot(df, aes(x = Temperature, y = log_GrowthRate, fill = Species)) +
  geom_boxplot()

ggplot(df, aes(x = Temperature, y = log_GrowthRate, color = Species)) +
  geom_point()

### 3. Define at least 4 models that explain the dependent variable "GrowthRate" ####
mod1 <- glm(formula = log_GrowthRate ~ Light, data=df)
summary(mod1)

mod2 <- glm(formula = log_GrowthRate ~ Humidity + Light, data = df)
summary(mod2) 

mod3 <- glm(formula = log_GrowthRate ~ Humidity * Light, data = df)
summary(mod3) 

mod4 <- glm(formula = log_GrowthRate ~ Humidity + Light + Species, data = df)
summary(mod4)

mod5 <- glm(formula = log_GrowthRate ~ Humidity * Light * Species, data=df)
summary(mod5)

# Plot the residuals
plot(mod1$residuals)
plot(mod2$residuals)
plot(mod3$residuals)
plot(mod4$residuals)
plot(mod5$residuals)

### 4. Calculate the mean sq. error of each model ####
mean(mod1$residuals^2)
mean(mod2$residuals^2)
mean(mod3$residuals^2)
mean(mod4$residuals^2)
mean(mod5$residuals^2)

# Make a list of all the models
mods <- list(mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4, mod5 = mod5)

# Apply "performance" function on all in the list and combine
map(mods, performance) %>% 
  reduce(full_join)

# Put the model into interpretable English:
report(mod5) # Looks like a pretty good model

## 5. Select the best model I tried ####
# mod5 looks the best to me because it has the lowest mean sq. error and the highest R-squared.
df2 <- add_predictions(df, mod5)

## 6. Adds predictions based on new hypothetical values for the independent variables used in the model ####
# Hypothetical values
newdf <- data.frame(Humidity = c("High", "Low", "High", "Low" ),
                    Light = c(30, 40, 50, 60),
                    Species = c("P.cornucopiae", "P.ostreotus", "P.ostreotus", "P.cornucopiae"))

# Make predictions
pred <- predict(mod5, newdata=newdf)

# Combine hypothetical input data with hypothetical predictions into one new data frame
hyp_preds <- data.frame(Humidity = newdf$Humidity,
                        Light = newdf$Light,
                        Species = newdf$Species,
                        pred = pred)

# Add new column showing whether a data point is real or hypothetical
df2$PredictionType <- "Real"
hyp_preds$PredictionType <- "Hypothetical"

# Join real data and hypothetical data (with model predictions)
fullpreds <- full_join(df2, hyp_preds)

## 7. Plot the predictions alongside real data. ####
# Showing just the light. The model looks like it predicts pretty well
# The predicted points follow the known data trend.
ggplot(fullpreds, aes(x = Light, y = pred, color = PredictionType)) +
  geom_point(aes(y = log_GrowthRate), color = "Black", width = 1) +
  geom_point() +
  theme_minimal() 

## Write the code you would use to model the data found in “/Data/non_linear_relationship.csv” with a linear model (there are a few ways of doing this) ####
nldf <- read_csv("../../Data/non_linear_relationship.csv")

modnl <- lm(formula = response ~ poly(predictor, 2), data = df)
summary(modnl)
