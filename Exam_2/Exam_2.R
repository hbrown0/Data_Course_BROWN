library(tidyverse)

##1. Read csv into data frame
unicef <- read.csv("unicef-u5mr.csv")

##2. Tidy the data
unicef_tidy <- unicef %>%
  pivot_longer(cols = starts_with("U5MR."), values_drop_na = TRUE,
    names_to = "Year",
    names_prefix = "U5MR.",
    names_transform = list(Year = as.integer),
    values_to = "U5MR")
    
##3. Plot each country’s U5MR over time
##Create a line plot (not a smooth trend line) for each country, facet by continent
ggplot(unicef_tidy, aes(x = Year, y = U5MR, group = CountryName)) +
      geom_line(alpha = 0.6, linewidth = 0.65, show.legend = FALSE) +
      facet_wrap(~ Continent) +
      labs(x = "Year", y = "U5MR") +
      theme_minimal()

##4. Save this plot as BROWN_Plot_1.png

##5. Create another plot that shows the mean U5MR for all the countries within a given continent at each year
##Another line plot (not smooth trendline), colored by continent

# Calculate mean U5MR by continent and year then graph
continent_mean <- unicef_tidy %>%
  group_by(Continent, Year) %>%
  summarise(continent_mean = mean(U5MR))

ggplot(continent_mean, aes(x = Year, y = continent_mean, color = Continent, group = Continent)) +
  geom_line(linewidth = 1) +
  labs(x = "Year", y = "Mean U5MR",
    color = "Continent") +
  theme_minimal()

##6. Save that plot as BROWN_Plot_2.png 

##7. Create three models of U5MR

#mod1 should account for only Year
mod1 <- glm(U5MR ~ Year, data = unicef_tidy)

#mod2 should account for Year and Continent
mod2 <- glm(U5MR ~ Year + Continent, data = unicef_tidy)

#mod3 should account for Year, Continent, and their interaction term
mod3 <- glm(U5MR ~ Year * Continent, data = unicef_tidy)

##8. Compare the three models with respect to their performance
#Your code should do the comparing
#Include a comment line explaining which of these three models you think is best

library(performance)

compare_performance(mod1, mod2, mod3)
##Based on the performance comparison, I think mod3 is the best because the R^2 is the highest

##9. Plot the 3 models’ predictions
library(modelr)

#Create predictions for each model
grid <- unicef_tidy %>%
  data_grid(Year, Continent) %>%
  gather_predictions(mod1, mod2, mod3)

#Plot
ggplot(grid, aes(x = Year, y = pred, color = Continent)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ model) +
  labs(x = "Year",
    y = "Predicted U5MR",
    color = "Continent") +
  theme_minimal()

##10. Save that plot as BROWN_Plot_3.png 





