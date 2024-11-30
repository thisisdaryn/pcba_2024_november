library(tidyverse)
library(rpart)
library(rpart.plot)
library(rsample)


cars2020 <- read_csv("data/cars2020.csv")

set.seed(1729)
cars_split <- initial_split(cars2020, prop = 0.7)
cars_train <- training(cars_split)
cars_test <- testing(cars_split)

model1 <- rpart(mpg~transmission+disp, data = cars_train)
rpart.plot(model1)

cars_test$model1 <- predict(model1, newdata = cars_test)

ggplot(data = cars_test,
       aes(x = mpg, y = model1)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  xlim(c(0,60)) + ylim(c(0,60))


model2 <- rpart(mpg~transmission+disp+atvType, data = cars_train)
rpart.plot(model2)
cars_test$model2 <- predict(model2, newdata = cars_test)


ggplot(data = cars_test,
       aes(x = mpg, y = model2)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  xlim(c(0,60)) + ylim(c(0,60))


model3 <- lm(mpg~disp, data = cars_train)

cars_test$model3 <- predict(model3, newdata = cars_test)


model4 <- lm(mpg~transmission, data = cars_train)
cars_test$model4 <- predict(model4, newdata = cars_test)


model5 <- lm(mpg~transmission+disp+atvType, data = cars_train)
cars_test$model5 <- predict(model5, newdata = cars_test)

cars_test_wide <- pivot_longer(cars_test, model1:model5,
                               names_to = "model_name", 
                               values_to = "model_prediction")

ggplot(data = cars_test_wide,
       aes(x = mpg, y = model_prediction)) + 
  geom_point(alpha = 0.3) + 
  geom_abline(intercept = 0, slope = 1) + 
  xlim(c(0,60)) + ylim(c(0,60)) + 
  facet_wrap(~model_name)

