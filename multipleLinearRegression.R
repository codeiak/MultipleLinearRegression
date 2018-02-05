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
# This is the long way of writing it
#regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State)
# Simpler way 
regressor = lm(formula = Profit ~ .,
               data = trainSet)

# Predicting the Test set result
y_pred = predict(regressor, newdata = testSet)

# Visualizing the sets
library(ggplot2)
ggplot() +
  geom_point(aes(x = trainSet$Profit, y = trainSet$R.D.Spend),
             color = 'red') +
  geom_line(aes(x = trainSet$Profit, y = predict(regressor, newdata = trainSet)),
            color = 'purple') +
  ggtitle('R&D Spend vs Profit (Training Set)') +
  xlab('Profit') +
  ylab('R&D Spend')

ggplot() +
  geom_point(aes(x = testSet$Profit, y = testSet$R.D.Spend),
             color = 'orange') +
  geom_line(aes(x = trainSet$Profit, y = predict(regressor, newdata = trainSet)),
            color = 'green') +
  ggtitle('R&D Spend vs Profit (Testing Set)') +
  xlab('Profit') +
  ylab('R&D Spend')