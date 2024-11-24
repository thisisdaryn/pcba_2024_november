library(tidyverse)

cars2020 <- read_csv("data/cars2020.csv")

manual <- filter(cars2020, transmission == "Manual")

count(cars2020, transmission)

transmission_fuel_df <- count(cars2020, transmission, fuel)

cyl_5 <- filter(cars2020, cyl >= 5)

# get all manual vehicles that also have 5 or more cylinders

manual_cyl5 <- filter(cars2020, transmission == "Manual",
                      cyl >= 5)

hist(cars2020$cyl, main = "Any title here", breaks = 5)

count(cars2020, cyl, sort = TRUE)

# how many automatic transmission cars have less than 6 gears

auto_l6 <- filter(cars2020, transmission == "Automatic",
                  gears < 6)
dim(auto_l6)

count(cars2020, transmission, gears)

## selecting only the model, mpg, transmission and cyl columns

cars_narrow <- select(cars2020, model, mpg, transmission, cyl)

cars_narrow2 <- select(cars2020, cyl, transmission, model, mpg)

# to move the transmission and disp variables to the left/front
cars_modified <- relocate(cars2020, transmission, disp)

# to rename 
cars_renamed <- rename(cars2020, displacement = disp)

## using the - to remove columns 

cars_alt <- select(cars2020, -startStop, -aspiration)

## sorting the data by mpg

cars_sorted <- arrange(cars2020, mpg)

cars_desc <- arrange(cars2020, desc(mpg))

cars_transmpg_sort <- arrange(cars2020, transmission, desc(mpg))





# create variable to indicate if vehicle has mpg >= 30

cars30 <- mutate(cars2020, above30 = if_else(mpg>=30, TRUE, FALSE))

hist(cars2020$mpg, main = "Distribution of fuel efficiency", 
     col = "royalblue", breaks = 50, xlab = "Miles per gallon",
     ylab = "# of vehicles")

## adding displacement and cylinders and creating a new column to join 
## the make and model columns

new_cars <- mutate(cars2020, new_col = disp + cyl,
                   other_col = paste(make, model)) |>
  relocate(other_col) |> 
  filter(transmission == "Automatic") |>
  arrange(desc(mpg))

# alternative to using the pipeline
temp1 <- mutate(cars2020, new_col = disp + cyl,
                other_col = paste(make, model))
temp2 <- relocate(temp1, other_col)
temp3 <- filter(temp2, transmission == "Automatic")
final <- arrange(temp3, desc(mpg))

## basic summary
report <- summarise(cars2020, num_cars = n(),
                    avg_mpg = mean(mpg, na.rm = TRUE))


manual_report <- summarise(manual, num_cars = n(),
                           med_mpg = median(mpg, na.rm = TRUE))

grouped_cars <- group_by(cars2020, transmission)
report <- summarise(grouped_cars, num_cars = n(),
                    avg_mpg = mean(mpg, na.rm = TRUE))

report2 <- group_by(cars2020, transmission) |>
  summarise(num_cars = n(), avg_mpg = mean(mpg, na.rm = TRUE))

# get the number of vehicles by make and class and avg mpg

report3 <- group_by(cars2020, make, class) |>
  summarise(num_cars = n(), avg_mpg = mean(mpg, na.rm = TRUE),
            .groups = "drop")

report4 <- cars2020 |> group_by(make, class) |> 
  summarise(num_cars = n(), avg_mpg = mean(mpg, na.rm = TRUE),
            .groups = "drop")











