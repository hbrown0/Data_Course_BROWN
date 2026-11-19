library(tidyverse)

###1. Load in dataset as a data frame 
utah = read.csv("Utah_Religions_by_County.csv")

###2. Tidy the data

##Keep country and population columns the same, merge religion and proportion columns into two
utah_tidy = utah %>%
  pivot_longer(
    cols = -c(County, Pop_2010), 
    names_to = "Religion",
    values_to = "Proportion") 

###3. Explore the data in figures

##Distribution of proportions for each religion across all counties using a boxplot
ggplot(utah_tidy, aes(x = reorder(Religion, Proportion, FUN = median), y = Proportion, fill = Religion)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  theme_minimal() +
  labs(
    title = "Distribution of Religious Proportions Across Utah Counties",
    x = "Religious Group",
    y = "Proportion of Population")

##Bar graphs for each country to see proportion of county by religion
ggplot(utah_tidy, aes(x = Religion, y = Proportion, fill = Religion)) +
  geom_col() +
  facet_wrap(~ County, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Religious Proportions by Individual Utah County",
    x = NULL,
    y = "Proportion"
  ) +
  theme(
    axis.text.x = element_blank(), # hides religion on the x-axis for easier visual
    legend.position = "bottom",
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.3, "cm"))

###3. Does population of a county correlate with the proportion of any specific religious group in that county?

##Plot of each religion proportion according to county population 
##I was recommended to use a log10 scale to account for the large population differences, which makes it easier to visualize 
utah_tidy %>%
  ggplot(aes(x = Pop_2010, y = Proportion)) +
  geom_point(alpha = 0.7, color = "#2b6cb0") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7, color = "#c53030") +
  scale_x_log10(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_y_continuous(labels = function(y) paste0(round(y * 100, 1), "%")) +
  facet_wrap(~ Religion, scales = "free_y") +
  labs(
    title = "County Population vs. Religious Group Proportion",
    x = "County Population (log10)",
    y = "Proportion in County"
  ) +
  theme_minimal()

###Answer to Q1: Based on these graphs, Catholic and Evangelical proportion seems strongly positively correlated with population, and Non-religious and LDS seem to have no correlation.

###4. Does proportion of any specific religion in a given county correlate with the proportion of non-religious people?

##Create separate data frame for non-religious proportions 
non_rel_col <- "Non.Religious"

non_rel_df <- utah_tidy %>%
  filter(Religion == non_rel_col) %>%
  select(County, non_rel_prop = Proportion)

##Plots of each religion's proportion compared to the non-religious proportions 
utah_tidy %>%
  filter(Religion != non_rel_col) %>%
  left_join(non_rel_df, by = "County") %>%
  ggplot(aes(x = Proportion, y = non_rel_prop)) +
  geom_point(alpha = 0.7, color = "#2f855a") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7, color = "#2b6cb0") +
  scale_x_continuous(labels = function(x) paste0(round(x * 100, 1), "%")) +
  scale_y_continuous(labels = function(y) paste0(round(y * 100, 1), "%")) +
  facet_wrap(~ Religion, scales = "free_x") +
  labs(
    title = "Religious Group Proportion vs. Non.Religious Proportion",
    x = "Specific Religion Proportion in County",
    y = paste0(non_rel_col, " Proportion in County")
  ) +
  theme_minimal()

###Answer for Q2: Based on the graphs, Religious and LDS proportions are strongly negatively correlated to the proportion of non-religious, and Muslim and Orthodox show no relation at all