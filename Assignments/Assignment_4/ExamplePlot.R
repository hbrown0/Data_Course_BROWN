##Loading necessary libraries
library(astsa)
library(ggplot2)
library(tidyr)

##Uploading dataframe
data(Lynx)  
data(Hare)

##Creating combined dataframe with both sets
predator_prey_df <- data.frame(
  Year = as.numeric(time(Hare)),
  Hare = as.numeric(Hare),
  Lynx = as.numeric(Lynx)
) %>%
  pivot_longer(cols = c(Hare, Lynx), names_to = "Species", values_to = "Population")

##Creating scatter plot with trendlines
ggplot(predator_prey_df, aes(x = Year, y = Population, color = Species, shape = Species)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("Hare" = "#1f77b4", "Lynx" = "#d62728")) +
  labs(
    title = "Snowshoe Hare & Canadian Lynx Population Cycles",
    subtitle = "(1845-1935)",
    x = "Year",
    y = "Population (Thousands)",
    color = "Species",
    shape = "Species"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "bottom"
  )

