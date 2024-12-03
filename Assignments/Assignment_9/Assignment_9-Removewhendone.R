## Load in packages ####
library(tidyverse)
library(GGally)
library(modelr)
library(easystats)
library(patchwork)

## Use the data set "/Data/GradSchool_Admissions.csv" ####
# Load in the dataset
df <- read_csv("../../Data/GradSchool_Admissions.csv")

# Clean the dataset. Changing the variable types
df$admit <- as.logical(df$admit)
df$rank <- as.factor(df$rank)

## Explore the data ####
# Look at the data structure
glimpse(df)

# Look at different combinations for looking for a quick look
ggpairs(df)
# * Talk about what I see in the plots from the ggpairs. 
# What I will focus on

# Plotting density admittance
# Kind of looks ridiculous
df %>% 
  ggplot(aes(x=admit, fill=rank)) +
  geom_density(alpha=.5) +
  theme_minimal()

# Other plots
ggplot(df, aes(x=gpa, y=admit, color=rank)) + # I don't love these
  geom_point()

ggplot(df, aes(y=gpa, x=admit, fill=rank)) +
  geom_boxplot()

ggplot(df, aes(x=gre, y=admit, color=rank)) + # I don't love these
  geom_point()

ggplot(df, aes(y=gre, x=admit, fill=rank)) +
  geom_boxplot()

# Define a few potential models with varying complexity:
names(df)

# Models
mod1 <- glm(data=df, formula = admit ~ gpa:gre + rank, family = "binomial")
summary(mod1)

mod2 <- glm(data=df, formula = admit ~ gpa + gre + rank, family = "binomial")
summary(mod2)

mod3 <- glm(data=df, formula = admit ~ gpa * gre * rank, family = "binomial")
summary(mod3)

# Compare the performance
compare_performance(mod1, mod2, mod3, rank=TRUE)

# Plot the comparisons
comps %>% 
  plot()

# Mean Residuals (Look for exact name from other assignment)
mean(mod1$residuals^2)
mean(mod2$residuals^2)
mean(mod3$residuals^2)

# All things considered, I am going to choose model 1 because it has a much higher 
# performance score than model 2 or model 3 at 66.67% (maybe put the others in here for
# showing the difference) and for the mean residuals, it is very similar to 
# model 3, but is a less complex model. 

# Add predictions from mod1 to a new dataframe
df2 <- add_predictions(df, mod1, type="response")

# Predict with model 1 how gre effects admittance
p1 <- ggplot(df2, aes(x=gre, y=pred, color=rank)) + # As gre goes up, the probability goes up
  geom_point() +
  geom_smooth(se=FALSE, method="lm") +
  labs(title="Probability of admittance based on GRE score",
       x="GRE",
       y="Prediction")

# Predict with model 1 how gpa effects admittance
p2 <- ggplot(df2, aes(x=gpa, y=pred, color=rank)) + # As gpa goes up, the probability goes up
  geom_point() +
  geom_smooth(se=FALSE, method="lm") +
  labs(title="Probability of admittance based on GPA score",
       x="GRE",
       y="Prediction")

p1 + p2 # Show them side by side
# Explain what I see here

