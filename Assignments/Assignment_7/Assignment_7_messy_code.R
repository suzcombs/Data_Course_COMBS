# Assignment 7 messy code
# Change this to "tidy" format using dplyr verbs

# There's an intuitive dplyr version for almost everything you see here.

# Note: Do not erase the original code, just comment it out and put your own equivalent code below each section
# i.e., change each line of indicated code to a tidy version that does the same thing.


library(tidyverse)

##########################
#        Part 1          #
##########################

# load data (wide format)
utah = read.csv("./Utah_Religions_by_County.csv")

# subset to only counties with buddhists observed
# Original: buddhist = utah[utah$Buddhism.Mahayana > 0,]
buddhist <- utah %>% 
  filter(Buddhism.Mahayana > 0) 

# order rows by population (descending)
# Original: buddhist = buddhist[order(buddhist$Pop_2010, decreasing = TRUE),]
buddhist <- buddhist %>% 
  arrange(desc(Pop_2010))


# write this new dataframe to a file
# Original: write.csv(buddhist, file = "./buddhist_counties.csv", row.names = FALSE, quote = FALSE)
write_csv(buddhist, "./buddhist_counties.csv")

## get group summaries of religiousity based on population ##

# divide each county into one of six groups based on populations
# note: keep these two lines the same in your updated code!
groups = kmeans(utah$Pop_2010,6) # clusters data into 6 groups based on proximity to mean of potential groups
utah$Pop.Group = groups$cluster # assigns a new variable to utah giving group for each county

# subset to each group and find summary stats on Religiosity for each
# Original: group1 = mean(utah[utah$Pop.Group == 1,]$Religious)
# Original: group2 = mean(utah[utah$Pop.Group == 2,]$Religious)
# Original: group3 = mean(utah[utah$Pop.Group == 3,]$Religious)
# Original: group4 = mean(utah[utah$Pop.Group == 4,]$Religious)
# Original: group5 = mean(utah[utah$Pop.Group == 5,]$Religious)
# Original: group6 = mean(utah[utah$Pop.Group == 6,]$Religious)
# New: Doing it down in next section all together.

# same, but mean population
# Original: group1.pop = mean(utah[utah$Pop.Group == 1,]$Pop_2010)
# Original: group2.pop = mean(utah[utah$Pop.Group == 2,]$Pop_2010)
# Original: group3.pop = mean(utah[utah$Pop.Group == 3,]$Pop_2010)
# Original: group4.pop = mean(utah[utah$Pop.Group == 4,]$Pop_2010)
# Original: group5.pop = mean(utah[utah$Pop.Group == 5,]$Pop_2010)
# Original: group6.pop = mean(utah[utah$Pop.Group == 6,]$Pop_2010)
# New: Doing it down in next section all together.

# make data frame of each group and mean religiosity
# Original: religiosity = data.frame(Pop.Group = c("group1","group2","group3","group4","group5","group6"),
           # Mean.Religiosity = c(group1,group2,group3,group4,group5,group6),
           # Mean.Pop = c(group1.pop,group2.pop,group3.pop,group4.pop,group5.pop,group6.pop))
religiosity <- utah %>% 
  group_by(Pop.Group) %>% 
  summarize(Mean.Religiosity = mean(Religious),
            Mean.Pop = mean(Pop_2010))


religiosity # take quick look at resulting table

# order by decreasing population
# Original: religiosity = religiosity[order(religiosity$Mean.Pop, decreasing = TRUE),]
religiosity <- religiosity %>% 
  arrange(desc(Mean.Pop))

religiosity # take quick look at resulting table


# plot that table (redo this using ggplot)
# Original: plot(x=religiosity$Mean.Pop,y=religiosity$Mean.Religiosity)
religiosity %>% 
  ggplot(aes(x=Mean.Pop, y=Mean.Religiosity)) +
  geom_point()


#####################################
#              Part 2               #
# Beginning to look at correlations #
# run this code without changing it #
# it's already in very tidy form    #
#####################################

# Look for correlations between certain religious groups and non-religious people
religions = names(utah)[-c(1:4)]

utah %>%
  pivot_longer(names_to = "Religion", values_to = "Proportion",religions) %>%
  ggplot(aes(x=Proportion,y=Religious)) + geom_point() + geom_smooth(method="lm") + lims(y=c(0,1)) +
  facet_wrap(~Religion,scales = "free") + theme_bw() + theme(panel.grid = element_blank(), strip.background = element_rect(fill="Gray"))


# Look through those plots and answer the following questions:
# 1.  Which religious group correlates most strongly in a given area with the proportion of non-religious people?
      # LDS

# 2.  What is the direction of that correlation?
      # Positive Relationship

# 3.  What can you say about the relationships shown here?
      # The points on the LDS plot is much tighter for all proportions in comparison to the other religions. There is also
      # a steep slope. 

# 4.  Examine the axis scales. How could you modify the code above to more accurately portray values on an "equal footing?"
      # It currently has free scales, but if you wanted to show exactly how they compare, then you could have the scales fixed.

# UPLOAD YOUR ANSWERS TO CANVAS
# DON'T FORGET TO PUSH YOUR TIDY CODE TO GITHUB AS WELL!