library(tidyverse)

chicago <- read_csv("data/ChicagoEmployees2024.csv") |>
  rename(title = `Job Titles`,       
         fp =     `Full or Part-Time`,
         salhour =  `Salary or Hourly`,
         typ_hours = `Typical Hours`,    
         salary =  `Annual Salary`,
         hourly_rate = `Hourly Rate`) |>
  mutate(dept2 = fct_lump_n(Department, 5)) |> 
  mutate(dept2 = str_replace(dept2, "CHICAGO", "")) |> 
  mutate(dept2 = str_replace(dept2, "DEPARTMENT OF", "")) |>
  mutate(dept2 = trimws(str_replace(dept2, "DEPARTMENT", "")))

dept_counts <- count(chicago, dept2)

ggplot(data = chicago,
       aes(x = dept2, fill = salhour)) + 
  geom_bar() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

ggplot(data = chicago,
       aes(x = salary, fill = dept2)) + 
  geom_histogram(color = "black") + 
  facet_wrap(~dept2, ncol = 1, scales = "free_y") +
  theme(legend.position = "bottom")

ggplot(data = chicago,
       aes(x = salary, y = dept2)) + 
  geom_boxplot()

## getting total salaries for all departments
salary_totals_df <- chicago |> 
  group_by(dept2) |>
  summarise(total_salaries = sum(salary, na.rm = TRUE), 
            .groups = "drop")

ggplot(data = salary_totals_df,
       aes(y = dept2, x = total_salaries)) + 
  geom_col(fill = "royalblue")






