# Exam 2
library(tidyverse)
library(dplyr)
library(ggplot2)
library(easystats)


# 1. Read in the unicef data:
df <-  read_csv("./unicef-u5mr.csv")


# 2. Get it into tidy format:
df_long <- df %>% 
  pivot_longer(starts_with("U5MR."),
               names_to = "Year",
               values_to = "U5MR",
               names_prefix = "U5MR.") %>%
  mutate(Year = as.numeric(Year)) %>% 
  filter(!is.na(U5MR)) 


# 3. Plot each country's U5MR over time:
df_long %>% 
  ggplot(aes(x = Year, y = U5MR, group = CountryName)) +
  geom_line() +
  facet_wrap(~Continent) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA) # My png was coming out with a gray background, so I needed a white fill.
  )


# 4. Save this plot as a png:
ggsave("./Combs_Plot_1.png", width = 12, height = 6, dpi = 300)


# 5. Create another plot that shows the mean U5MR for all the countries within a given continent at each year:
df_cont_yr <- df_long %>% # Create new datafrane with means for U5MR for all continents for each year
  group_by(Continent, Year) %>% 
  summarize(Mean_U5MR = mean(U5MR))

ggplot(df_cont_yr, aes(x = Year, y = Mean_U5MR, color = Continent)) + # Create the plot using the new dataframe
  geom_line() +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA) # My png was coming out with a gray background, so I needed a white fill.
  )


# 6. Save the plot as a png
ggsave("./Combs_Plot_2.png", width = 8, height = 6, dpi = 300)


# 7. Create three models of U5MR:

mod1 <- glm(data = df_long,  # mod1 should account for only Year
           formula = U5MR ~ Year)
summary(mod1)

mod2 <- glm(data = df_long,  # mod2 should account for Year and Continent
            formula = U5MR ~ Year + Continent)
summary(mod2)

mod3 <- glm(data = df_long,  # mod3 should account for Year, Continent, and their interaction term
            formula = U5MR ~ Year * Continent)
summary(mod3)


# 8. Compare the three models with respect to their performance
compare_models(mod1, mod2, mod3)
compare_performance(mod1, mod2, mod3)
  # Mod 3 is the best because it has the low AIC weights and BIC weights. The lowest RMSE. R2 is also the highest. 


# 9. Plot the 3 models' predictions like the example:
df_long$pred <- predict(mod1, df_long)
df_long$pred2 <- predict(mod2, df_long)
df_long$pred3 <- predict(mod3, df_long)

df_long %>% 
  ggplot(aes(x = Year, y = pred)) +
  geom_line()

df_long %>% 
  ggplot(aes(x = Year, y = pred2, group = Continent, color = Continent)) +
  geom_line()

df_long %>% 
  ggplot(aes(x = Year, y = pred3, group = Continent, color = Continent)) +
  geom_line()

df_long %>% 
  pivot_longer(starts_with("pred"),
               names_to = "Name",
               values_to = "Predicted_U5MR") %>% 
  ggplot(aes(x = Year, y = Predicted_U5MR, color = Continent)) +
  geom_line() +
  facet_wrap(~ Name) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA) # My png was coming out with a gray background, so I needed a white fill.
  )

# Saving as a png
ggsave("./Combs_Plot_3.png", width = 12, height = 6, dpi = 300)

