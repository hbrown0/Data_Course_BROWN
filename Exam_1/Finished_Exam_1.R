library(tidyverse)
library(ggplot2)
library(dplyr)

##I. Read the cleaned_covid_data.csv file into an R data frame.
df <- read.csv("cleaned_covid_data.csv")

##II. Subset the data set to just show states that begin with "A" and save this as an object called A_states.
A_states <- df[grepl("^A", df$Province_State), ]

##III. Create a plot _of that subset_ showing Deaths over time, with a separate facet for each state.
A_states$Last_Update <- as.Date(A_states$Last_Update)

ggplot(A_states, aes(x = Last_Update, y = Deaths)) +
  geom_point(color = "steelblue", size = 0.25) +
  geom_smooth(method = "loess", color = "darkorange", se = FALSE, size = 1) +
  facet_wrap(~ Province_State, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Deaths Over Time for 'A' States",
    x = "Date",
    y = "Number of Deaths"
  )

##IV. (Back to the full dataset) Find the "peak" of Case_Fatality_Ratio for each state and save this as a new data frame object called state_max_fatality_rate.
state_max_fatality_rate <- df %>%
  group_by(Province_State) %>%
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>%
  arrange(desc(Maximum_Fatality_Ratio))

##V. Use that new data frame from task IV to create another plot.
state_max_fatality_rate <- state_max_fatality_rate %>%
  mutate(Province_State = factor(Province_State, levels = Province_State))

ggplot(state_max_fatality_rate, aes(x = Province_State, y = Maximum_Fatality_Ratio)) +
  geom_col(fill = "steelblue") + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) 
  ) +
  labs(
    title = "Peak Case Fatality Ratio by State",
    x = "Province / State",
    y = "Maximum Fatality Ratio"
  )

##VI. Using the FULL data set, plot cumulative deaths for the entire US over time
us_total_deaths <- df %>%
  mutate(Last_Update = as.Date(Last_Update)) %>% 
  group_by(Last_Update) %>% 
  summarize(Total_Deaths = sum(Deaths, na.rm = TRUE))

ggplot(us_total_deaths, aes(x = Last_Update, y = Total_Deaths)) +
  geom_line(color = "darkred", size = 1.2) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Cumulative COVID-19 Deaths in the United States Over Time",
    x = "Date",
    y = "Total Cumulative Deaths"
  )