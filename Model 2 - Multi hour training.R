library(recipes)
library(keras)

#source("LoadData.R")
source("Utils.R")

# Model 2: Training the model to predict based on only 'n' previous time steps

numHours <- 3

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

train_Y <- reshapeForLSTM(train_data, numTimeSteps = 4, columnsToExclude = "nextPollution")
train_X <- reshapeForLSTM(train_data[,-8])
test_Y <- as.matrix(test_data$nextPollution)
test_X <- reshapeForLSTM(test_data, numTimeSteps = 4, columnsToExclude = "nextPollution")
