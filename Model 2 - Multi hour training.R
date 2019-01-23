library(recipes)
library(keras)
library(lubridate)
library(tidyverse)

#source("LoadData.R")
source("Utils.R")

# Model 2: Training the model to predict based on only 'n' previous time steps

# Initial preparation is exactly as per model 1

# Scaling is done based on all data
recipe <- recipe(nextPollution ~ pollution + dew + temp + press + wnd_dir + wnd_spd + snow + rain, data = pollution)

# Scale numeric variables to [0,1] and one hot encode the factor.
scaledAndCentered <- recipe %>%
  step_range(all_numeric()) %>%
  step_dummy(wnd_dir)

trained_rec <- prep(scaledAndCentered, training = pollution)

train_data <- bake(trained_rec, newdata = pollution.train)
test_data  <- bake(trained_rec, newdata = pollution.test)

train_Y <- as.matrix(tail(train_data$nextPollution, -4))
train_X <- reshapeForLSTMLoop(train_data, numTimeSteps = 4, columnsToExclude = "nextPollution")
test_Y <- as.matrix(tail(test_data$nextPollution, -4))
test_X <- reshapeForLSTMLoop(test_data, numTimeSteps = 4, columnsToExclude = "nextPollution")


model <- keras_model_sequential()

model %>%
  layer_lstm(units = 50, input_shape = c(dim(train_X)[2], dim(train_X)[3])) %>%
  layer_dense(units = 1)

model %>%
  compile(loss = "mae", optimizer = "adam")

model %>%
  fit(train_X, train_Y, 
      epochs = 50, batch_size = 72,
      validation_data = list(test_X, test_Y), verbose = 2,
      shuffle = FALSE)

yhat = model %>% predict(test_X)

ranges <- trained_rec$steps[[1]]$ranges
yhat <- c(rep(NA, 4), yhat * ranges[2,1])

rmse <- sqrt(mean((yhat - pollution.test$nextPollution), na.rm = TRUE))

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

