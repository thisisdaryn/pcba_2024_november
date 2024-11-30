library(tidyverse)

penguins <- read_csv("data/penguins.csv")

# plotting bill_depth (y) vs flipper_length (x)
ggplot(data = penguins,
       aes(x = flipper_length_mm, y = bill_depth_mm)) + 
  geom_point() + theme_minimal() + 
  labs(title = "My penguin plot",
       subtitle = "Scientists observed 344 penguins")

ggplot(data = penguins,
       aes(x = flipper_length_mm, y = bill_depth_mm,
           colour = species)) + 
  geom_point() + theme_minimal() + 
  labs(title = "My penguin plot") + 
  theme(legend.position = "bottom")

## adding different shapes to distinguish between males and females
ggplot(data = penguins,
       aes(x = flipper_length_mm, y = bill_depth_mm,
           colour = species, shape = sex)) + 
  geom_point() + theme_minimal() + 
  labs(title = "My penguin plot") + 
  theme(legend.position = "bottom")


## Making a histogram of bill lengths of all penguins

ggplot(data = penguins,
       mapping = aes(x = bill_length_mm)) + 
  geom_histogram(fill = "purple", colour = "black")

# using fill to show different species
ggplot(data = penguins,
       mapping = aes(x = bill_length_mm, fill = species)) + 
  geom_histogram(color = "black")

ggplot(data = penguins,
       mapping = aes(x = bill_length_mm, fill = species)) + 
  geom_histogram(color = "black") + 
  facet_wrap(~species, ncol = 1) + 
  theme(legend.position = "top")


## Using boxplots to show distributions (comparing groups)
ggplot(penguins, 
       aes(x = species, y = bill_length_mm)) + 
  geom_boxplot()

## adding species to the boxplots
ggplot(penguins,
       aes(x = species, y = bill_length_mm, color = sex)) +
  geom_boxplot() 

## using a violin plot
ggplot(penguins,
       aes(x = species, y = bill_length_mm)) + 
  geom_violin()

library(ggbeeswarm)

ggplot(penguins, 
       aes(x = species, y = bill_length_mm, color = sex)) +
  geom_beeswarm()













