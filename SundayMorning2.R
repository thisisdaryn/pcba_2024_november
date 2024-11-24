library(tidyverse)

chi_emps <- read_csv("data/ChicagoEmployees2024.csv", 
                     show_col_types = FALSE)

# Question: Can we make a table showing for each department a) how many salaried
# employees there are and what the total annual salaries are?

salary_report <- chi_emps |> 
  filter(`Salary or Hourly` == "SALARY") |> 
  group_by(Department) |> 
  summarise(num_employees = n(), 
            total_salary = sum(`Annual Salary`), 
            .groups = "drop")

chi2 <- rename(chi_emps, salhourly = 5, salary = 7) |> 
  filter(salhourly == "SALARY") |>
  group_by(Department) |>
  summarise(num_employees = n(),
            total_salary = sum(salary),
            .groups = "drop") |> 
  arrange(desc(total_salary))

sorted_df <- arrange(chi_emps, desc(`Annual Salary`))
