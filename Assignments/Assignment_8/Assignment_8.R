library(modelr)
library(easystats)
library(broom)
library(tidyverse)

shroom = read.csv('mushroom_growth.csv')

# Exploratory plots
ggplot(shroom, aes(x = Temperature,y = GrowthRate)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()

ggplot(shroom, aes(x = Nitrogen,y = GrowthRate)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()

ggplot(shroom, aes(x = Nitrogen, y = GrowthRate, color = Species)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  facet_grid(Light ~ Humidity, labeller = label_both) +
  theme_minimal()

# Define the 4 models

models = list(
  mod1 = glm(GrowthRate ~ Light + Nitrogen + Temperature + Species + Humidity, data = shroom),
  mod2 = glm(GrowthRate ~ Light * Humidity + Nitrogen + Temperature + Species, data = shroom),
  mod3 = glm(GrowthRate ~ Light * Humidity * Species + Nitrogen + Temperature, data = shroom),
  mod4 = glm(GrowthRate ~ Light * Humidity * Species * Nitrogen, data = shroom))

#Compare models using MSE
mse_results <- tibble(model = names(models),
  mse = map_dbl(models, ~ mean((shroom$GrowthRate - fitted(.x))^2)),
  rmse = sqrt(mse)) %>%
  arrange(mse)

print(mse_results)

##Best model is mod4 because it has the lowest mse 

#Create hypothetical data from mod4 predictions
new_shroom <- expand_grid(Light = c(0, 10, 20),
  Humidity = c("Low", "High"),
  Species = unique(shroom$Species),
  Nitrogen = seq(0, 45, by = 1)) %>%
  mutate(pred_GrowthRate = predict(mod4, newdata = ., type = "response"))

#Plot hypothetical by real data 
ggplot() +
  geom_point(data = shroom, aes(x = Nitrogen, y = GrowthRate, color = Species, shape = "Real Data"),
    size = 2.4,
    alpha = 0.85) +
  geom_line(data = new_shroom, aes(x = Nitrogen, y = pred_GrowthRate, color = Species, linetype = "mod4 Prediction"),
    linewidth = 1.15) +
  facet_grid(Light ~ Humidity, labeller = label_both) +
  labs(x = "Nitrogen",
    y = "GrowthRate",
    color = "Species",
    shape = "Observations",
    linetype = "Model") +
  theme_minimal()
