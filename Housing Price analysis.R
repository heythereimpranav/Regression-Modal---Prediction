library(tidyverse)
library(ISLR2)
library(lattice)
library(caret)
library(leaps)
library(stargazer)

house <- Housing

#Summary of Housing data

summary(house)

# Correlation between area, bedrooms,stories,parking,bathrooms

cor(house$price, house$area)
cor(house$price, house$bedrooms)
cor(house$price, house$stories)
cor(house$price, house$parking)
cor(house$price, house$bathrooms)

# Linear regression model

Model_1 <- lm(price ~ area + bathrooms + stories+ parking, data = house)
coef(Model_1)

# Diagnostics

Model_1Resid <- Model_1$residuals
Model_1Fitted <- Model_1$fitted.values
plot(Model_1Fitted, Model_1$Resid)

hist(Model_1Resid)
qqnorm(Model_1Resid)

# Model Selection

Model_2 <- train(
  form = price ~ area + bathrooms + stories + parking,
  data = house,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)
Model_2

# Subset model

SubsetModal <- regsubsets(price ~  bathrooms + stories + parking, data = house)
plot(SubsetModal, scale = "adjr2")

#RMSE value of subset model

Model_3 <- train(
  form = price ~ bathrooms + stories + parking,
  data = house,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)
Model_3

#Prediction

predictionVariable <- data.frame(area = c(5000,6000,7000), bathrooms = c(2,3,4), stories = c(1,2,3), parking = c(1,2,3))
predict(Model_1, predictionVariable)



