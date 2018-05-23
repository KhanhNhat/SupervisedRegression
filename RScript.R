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

#Calculate RMSE and standard deviation of female_unemployment
#A model is good when RMSE < sd
(rmse = sqrt(mean(unemployment$residuals^2)))
sd(unemployment$female_unemployment)

#Calculate R Squared = 1 - SSE/SST
female_mean = mean(unemployment$female_unemployment)

#Sum of Square Error: SSE
(SSE = sum(unemployment$residuals^2))

#Sum of Square Total
(SST = sum((unemployment$female_unemployment - female_mean)^2))

(RSquare = 1 - SSE/SST)


#Study Categorical Inputs
flowers = read.csv('flower.csv', row.names = 1)

mmat = model.matrix(Flowers ~ Intensity + Time, data = flowers)

flower_model = lm(Flowers ~ Intensity + Time, data = flowers)

flowers$Predictions = predict(flower_model, flowers)

ggplot(flowers, aes(x = Predictions, y = Flowers)) +
  geom_point() +
  geom_abline(col = 'blue', size = 1)

#Study Interaction
alcohol = read.csv('alcohol.csv', row.names = 1)

#Create a formula with main effect only
fmla_mainEffect = as.formula(Metabol ~ Gastric + Sex)

#Create a formula with Gastric as a main effect and the interaction between Gastric and Sex
fmla_interaction = as.formula(Metabol ~ Gastric + Gastric:Sex)

#Now, create model
model_mainEffect = lm(fmla_mainEffect, data = alcohol)
model_interaction = lm(fmla_interaction, data = alcohol)

#Doing cross validation to qualify each model
library(vtreat)

set.seed(34245)
splitPlan = kWayCrossValidation(nrow(alcohol), 3, NULL, NULL)

alcohol$pred_mainEffect = 0
for(i in 1:3){
  split = splitPlan[[i]]
  m_mainEffect = lm(fmla_mainEffect, data = alcohol[split$train, ])
  alcohol$pred_mainEffect[split$app] = predict(m_mainEffect, alcohol[split$app, ])
}

alcohol$pred_interaction = 0
for(i in 1:3){
  split = splitPlan[[i]]
  m_interaction = lm(fmla_interaction, data = alcohol[split$train, ])
  alcohol$pred_interaction[split$app] = predict(m_interaction, alcohol[split$app, ])
}

#Calculate RSME for each model
alcohol %>%
  gather(key = modelType, value = pred, pred_mainEffect, pred_interaction) %>%
  group_by(modelType) %>%
  mutate(residuals = Metabol - pred) %>%
  summarise(rsme = sqrt(mean(residuals^2)))
