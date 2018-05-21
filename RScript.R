#Load necessary package
library(tidyverse)
library(broom)
library(WVPlots)

#Create a simple linear regression model with lm function
unemployment_model = lm(female_unemployment ~ male_unemployment, data = unemployment)

#Compare the predicted and real value of female_unemployment
unemployment$female_Predicted = predict(unemployment_model, unemployment)

#Take a look to this model
#Simple view
unemployment_model

#More detail with summary function
summary(unemployment_model)

#Or using broom::glance
glance(unemployment_model)

#There are many ways to compare
#1. Predicted vs Real values (the green line means: predicted == real values)
ggplot(unemployment, aes(x = female_Predicted, y = female_unemployment)) +
  geom_point() +
  geom_abline(col = 'green', size = 1)

#2. Draw a real data point (female ~ male) and a regression line(blue)
ggplot(unemployment, aes(x = male_unemployment, y = female_unemployment)) +
  geom_point() + 
  geom_line(aes(y = female_Predicted), col = 'blue', size = 1)

#3. Compare predicted value and its residuals
unemployment$residuals = unemployment$female_unemployment - unemployment$female_Predicted
ggplot(unemployment, aes(x = female_Predicted, y = residuals)) +
  geom_pointrange(aes(ymin = 0, ymax = residuals)) +
  geom_hline(yintercept = 0, linetype = 3) +
  ggtitle('Residuals vs Linear Model Prediction')

#Study more about this model
GainCurvePlot(frame = unemployment, xvar = 'male_unemployment', truthVar =  'female_unemployment', 
              title = 'Unemployment Model')
