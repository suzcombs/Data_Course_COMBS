library(tidyverse)

# YOUR TASKS:

# I. Read the cleaned_covid_data.csv file into an R data frame. (20 pts)
df <- read_csv("./cleaned_covid_data.csv", show_col_types = FALSE)


# II. Subset the data set to just show states that begin with "A" and save this as an object called A_states. (20 pts)**
A_states <- filter(df, grepl("^A", Province_State)) # Filter used in *tidyverse* suite of packages. Only showing states that begin with "A"


# III. Create a plot of that subset showing Deaths over time, with a separate facet for each state. (20 pts)**
deaths_over_time <- ggplot(A_states, aes(x=Last_Update, y=Deaths)) + # Deaths over time
  geom_point() + # Creates a scatter plot
  geom_smooth(method = "loess", se = FALSE) + # Add loess curves WITHOUT standard error shading
  facet_wrap(~ Province_State, scales = "free") # Keep scales "free" in each facet
deaths_over_time # Show the plot


# IV. (Back to the full dataset) Find the "peak" of Case_Fatality_Ratio for each state 
  # and save this as a new data frame object called state_max_fatality_rate. (20 pts)**
state_max_fatality_rate <- df %>% 
  group_by(Province_State) %>% 
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>% # New data frame with Province_State and Maximum_Fatality_Ratio (2 Columns). Remove missing values
  arrange(desc(Maximum_Fatality_Ratio)) # Arrange the new data frame in descending order by Maximum_Fatality_Ratio

  
# V. Use that new data frame from task IV to create another plot. (20 pts)**
ggplot(state_max_fatality_rate, aes(x = reorder(Province_State, -Maximum_Fatality_Ratio), y = Maximum_Fatality_Ratio)) + # X-axis Province_State, Y-axis Maximum_Fatality_Ratio. Arrange in descending order
  geom_bar(stat = "identity") + # Bar plot
  theme(axis.text.x = element_text(angle=90)) + # X-axis labels turned to 90 deg to be readable
  labs(x = "Province_State")

# VI. (BONUS 10 pts) Using the FULL data set, plot cumulative deaths for the entire US over time
cumulative_fatality_rate <- df %>% 
  group_by(Last_Update) %>% 
  summarize(Cumulative_Fatality_Ratio = sum(Deaths)) 

ggplot(cumulative_fatality_rate, aes(x=Last_Update, y=Cumulative_Fatality_Ratio)) +
  geom_point(stat = "identity") +
  # geom_point() +
  theme(axis.text.x = element_text(angle=90))

