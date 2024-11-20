### Assignment 6 ###
library(tidyverse)
library(janitor)
library(gganimate)
library(dplyr)
library(ggplot2)

# Import the untidy data set
dat <- read_csv("../../Data/BioLog_Plate_Data.csv")

# 1. Clean the data into tidy (long) form
dat_long <- dat %>% 
  pivot_longer(starts_with("Hr_"),
               names_to = "time",
               values_to = "Absorbance") %>% 
  mutate(time = case_when(time == 'Hr_24' ~ 24,
                        time == 'Hr_48' ~ 48,
                        time == 'Hr_144' ~ 144)) %>% 
  clean_names() # Remove the space in the word

# 2. Create a new column specifying whether a sample is from soil or water
dat_long <- dat_long %>% 
  mutate(type = case_when(sample_id %in% c("Clear_Creek", "Waste_Water") ~ "Water",
                                 sample_id %in% c("Soil_1", "Soil_2") ~ "Soil"))

# 3. Generate a plot matching the one shown in the assignment
dat_long$type <- as.factor(dat_long$type)
dat_long$substrate <- as.factor(dat_long$substrate)

dat_long %>% 
  ggplot(aes(x = time, y = absorbance, color = type)) +
  geom_smooth(method = "loess", formula = y ~ x, se=FALSE) +
  facet_wrap(~substrate) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA)) # My png was coming out with a gray background, so I needed a white fill.

# Save as a .png
ggsave("./absorbance_over_hr_plot.png", width = 12, height = 6, dpi = 300)

# Animated plot that matches the one in the assignment
dat_long_mean <- dat_long %>% 
  filter(substrate == "Itaconic Acid") %>% 
  group_by(sample_id, dilution, substrate, time, type) %>% 
  summarize(mean_absorbance = mean(absorbance, na.rm = TRUE))

dat_long_mean %>% 
  ggplot(aes(x = time, y = mean_absorbance, color = sample_id, 
             group = sample_id)) + 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) + 
  facet_wrap(~dilution) + 
  theme_minimal() + 
  theme( plot.background = element_rect(fill = "white", color = NA) # Ensures white background 
         )

# I can't get the animation to work
# Gives: Error in rep(self$default, ncol(d)) : invalid 'times' argument
ggsave("./itaconic_acid.png", width = 12, height = 6, dpi = 300)
