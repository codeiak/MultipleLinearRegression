# Multiple Linear Regression model

# Importing the data sets
dataset = read.csv('50_Startups.csv')

# Encoding categorical data
dataset$State = factor(dataset$State, 
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))

# Splitting the data sets into training and testing sets
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
trainSet = subset(dataset, split == TRUE)
testSet = subset(dataset, split == FALSE)

# Fitting Multiple linear regression to the training set

# Predicting the Test set result
y_pred = predict(regressor, newdata = testSet)

# Building the optimal model using Backward Elimination
backwardElimination <- function(x, sl){
  numVariables = length(x)
  for(i in c(1:numVariables)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVariable = max(coef(summary(regressor))[c(2:numVariables), "Pr(>|t|)"])
    if(maxVariable > sl){
      j = which(coef(summary(regressor))[c(2:numVariables), "Pr(>|t|)"] == maxVariable)
      x = x[, -j]
    }
    numVariables = numVariables - 1
  }
  return(summary(regressor))
}

SL = 0.05
dataset = dataset[, c(1, 2, 3, 4, 5)]
backwardElimination(trainSet, SL)
