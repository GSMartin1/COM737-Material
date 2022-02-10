##### Neural Networks  -------------------

## Example: Modeling the expenses of insurance  ----

## Step 1: Collecting data

## Step 2: Exploring and preparing the data ----
# read in data and examine structure

insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE)

str(insurance)

# Convert factor to numeric features
insurance$sex <- as.numeric(insurance$sex)
insurance$smoker <- as.numeric(insurance$smoke)
insurance$region <- as.numeric(insurance$region)

# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame (you may also try to used un-normalised data to see how the results look like)
insurance_norm <- as.data.frame(lapply(insurance, normalize))

# confirm that the range is now between zero and one
summary(insurance_norm$expenses)

# compared to the original minimum and maximum
summary(insurance$expenses)

# create training and test data (you may wish to use k-fold cross validation to get a validated result)
insurance_train <- insurance_norm[1:669, ]
insurance_test <- insurance_norm[670:1338, ]

## Step 3: Training a model on the data ----
# train the neuralnet model
library(neuralnet)

# simple ANN with only a single hidden neuron
set.seed(12345) # to guarantee repeatable results
insurance_model <- neuralnet(formula = expenses ~ age + sex + bmi + children + smoker + region, data = insurance_train)

# visualize the network topology
plot(insurance_model)

## Step 4: Evaluating model performance ----
# obtain model results
model_results <- compute(insurance_model, insurance_test[1:6])
# obtain predicted strength values
predicted_strength <- model_results$net.result
# examine the correlation between predicted and actual values
cor(predicted_strength, insurance_test$expenses)

## Step 5: Improving model performance ----
# a more complex neural network topology with 5 hidden neurons
set.seed(12345) # to guarantee repeatable results
insurance_model2 <- neuralnet(expenses ~ age + sex + bmi + children + smoker + region, data = insurance_train, hidden = 5)
# plot the network
plot(insurance_model2)

# evaluate the results as we did before
model_results2 <- compute(insurance_model2, insurance_test[1:6])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, insurance_test$expenses)
