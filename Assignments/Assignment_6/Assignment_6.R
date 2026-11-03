library(tidyverse)
dat <- read.csv('BioLog_Plate_Data.csv')

##1. Cleans this data into tidy (long) form
tidy_dat <- dat %>%
  pivot_longer(
    cols = c(Hr_24, Hr_48, Hr_144),
    names_to = "Hour",
    values_to = "Absorbance",
    names_prefix = "Hr_")

##2. Creates a new column specifying whether a sample is from soil or water
tidy_source <- tidy_dat %>%
  mutate(
    sample_source = case_when(
      grepl("soil|S_", Sample.ID, ignore.case = TRUE) ~ "Soil",
      grepl("water|W_|Creek", Sample.ID, ignore.case = TRUE) ~ "Water",))


##3. Generates a plot that matches this one (note just plotting dilution == 0.1):
tidy_source %>%
  filter(Dilution == 0.1) %>%
  mutate(Hour = as.numeric(as.character(Hour))) %>% # 1. Convert Hour to numeric for continuous regression
  ggplot(aes(x = Hour, y = Absorbance, color = sample_source)) + # 2. REMOVED global group = Sample.ID here
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.0) +  # Switched from "lm" to "loess" for a curved fit
  facet_wrap(~ Substrate) +
  facet_wrap(~ Substrate) +
  theme_minimal() +
  labs(title = "Absorbance Over Time by Substrate (Dilution = 0.1)",
    x = "Time",
    y = "Absorbance",
    color = "Type")

##4. Generates an animated plot that matches this one (absorbance values are mean of all 3 replicates for each group):
##This plot is just showing values for the substrate “Itaconic Acid”

install.packages("gganimate")
install.packages("gifski")
library(gganimate)

# Filter
animated_data <- tidy_dat %>%
  filter(Substrate == "Itaconic Acid") %>%
  mutate(Hour = as.numeric(as.character(Hour))) %>%
  group_by(Dilution, Sample.ID, Hour) %>%
  summarize(Mean_Absorbance = mean(Absorbance, na.rm = TRUE), .groups = "drop")

# Plot
p <- animated_data %>%
  ggplot(aes(x = Hour, y = Mean_Absorbance, color = Sample.ID, group = Sample.ID)) +
  geom_line(linewidth = 1.0) +
  theme_minimal() +
  labs(title = "Mean Absorbance Over Time: Itaconic Acid",
    x = "Time",
    y = "Mean Absorbance",
    color = "Sample ID") +
  facet_wrap( ~ Dilution) +
  transition_reveal(Hour) 

# Animation
animate(p, renderer = gifski_renderer(), duration = 8, fps = 12)

