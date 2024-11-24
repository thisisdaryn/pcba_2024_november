library(tidyverse)

penguins <- read_csv("data/penguins.csv", 
                     show_col_types = FALSE)
count(penguins, species, island)

plot(bill_length_mm~flipper_length_mm, data = penguins,
     main = "My penguin plot")

ggplot(data = penguins, 
       mapping = aes(x = flipper_length_mm,
                     y = bill_length_mm, 
                     color = species,
                     shape = sex)) + 
  geom_point() + 
  theme_minimal() + 
  labs(title = "My penguin plot",
       subtitle = "Some scientists studied some penguins and I made a graph",
       y = "Bill length", x = "Flipper length",
       caption = "2024") + 
  theme(legend.position = "bottom")
