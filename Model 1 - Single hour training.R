library(recipes)
library(keras)
library(lubridate)
library(tidyverse)
library(magrittr)

#source("LoadData.R")

# Model 1: Training the model to predict based on only 1 previous time step

# Scaling is done based on all data
recipe <- recipe(nextPollution ~ pollution + dew + temp + press + wnd_dir + wnd_spd + snow + rain, data = pollution)

# Scale numeric variables to [0,1] and one hot encode the factor.
scaledAndCentered <- recipe %>%
  step_range(all_numeric()) %>%
  step_dummy(wnd_dir)

trained_rec <- prep(scaledAndCentered, training = pollution)

train_data <- bake(trained_rec, newdata = pollution.train)
test_data  <- bake(trained_rec, newdata = pollution.test)

# Cast to matrix and set dimensions
train_Y <- as.matrix(train_data$nextPollution)
train_X <- as.matrix(train_data[,-8])
test_Y <- as.matrix(test_data$nextPollution)
test_X <- as.matrix(test_data[,-8])

dim(train_X) <- c(nrow(train_X),1,ncol(train_X))
dim(test_X) <- c(nrow(test_X),1,ncol(test_X))

# Define the model

model <- keras_model_sequential()

model %>%
  layer_lstm(units = 50, input_shape = c(dim(train_X)[2], dim(train_X)[3])) %>%
  layer_dense(units = 1)

model %>%
  compile(loss = "mae", optimizer = "adam")

history <- model %>%
  fit(train_X, train_Y, 
      epochs = 50, batch_size = 72,
      validation_data = list(test_X, test_Y), verbose = 2,
      shuffle = FALSE)

plot(history)

yhat = model %>% predict(test_X)

ranges <- trained_rec$steps[[1]]$ranges
yhat <- yhat * ranges[2,1]

rmse <- sqrt(mean((yhat - pollution.test$nextPollution)^2))

pollution.test$predictedValue <- yhat

# Simple plot of predicted vs actual for a short period...
pollution.test %>%
  filter(date > "2012-01-01", date <= "2012-01-31") %>%
  mutate(week = week(date)) %>%
  select(date, week, nextPollution, predictedValue) %>%
  gather(type, value, -date, -week) %>%
  ggplot(., aes(date, value, colour = type)) + 
  geom_line() + 
  facet_wrap(~ week, scales = "free")
