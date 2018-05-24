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

ggplot(alcohol, aes(x = Metabol, y = pred_mainEffect)) +
  geom_point(col = 'blue') +
  geom_point(aes(y = pred_interaction), col = 'green') +
  geom_abline()


#Study Logistic Regression to predict Probabiliy
sparrow = read.csv('sparrow.csv', row.names = 1)
sparrow$survived = ifelse(sparrow$status == 'Survived', TRUE, FALSE)

sparrow_model = glm(survived ~ total_length + weight + humerus, data = sparrow, family = 'binomial')

summary(sparrow_model)
sparrow_model_prop = glance(sparrow_model)

#Calculate pseudo-R2:
1 - sparrow_model_prop$deviance/sparrow_model_prop$null.deviance

#Predict probability
sparrow$pred = predict(sparrow_model, type = 'response')

#To calculate the accuracy, accuracy = total of correct predict/total of all predict
#This accuracy can change by modify the threshold, in this case, threshold is 0.5
table(sparrow$survived, ifelse(sparrow$pred > 0.5, TRUE, FALSE))

GainCurvePlot(sparrow, 'pred', 'survived', title = 'Sparrow survival model')

#Use Poisson or QuasiPoisson to predict count variable ( 0 - Inf)
mean(bikesJuly$cnt)
var(bikesJuly$cnt)

#Because var >> mean, so that we use 'quasipoisson'
bike_model = glm(cnt ~ ., data = bikesJuly[-c(11,12)], family = 'quasipoisson')

#Calculate pseudo R2
bike_model_prop = glance(bike_model)
1 - bike_model_prop$deviance/bike_model_prop$null.deviance

#Now use model to predict bike rents in August
bikesAugust$pred = predict(bike_model, bikesAugust, type = 'response')

#Calculate RSME
bikesAugust %>%
  mutate(residual = pred - cnt) %>%
  summarise(rsme = sqrt(mean(residual^2)))

#Plot prediction vs actual
ggplot(bikesAugust, aes(x = pred, y = cnt)) +
  geom_point() +
  geom_abline(col = 'blue', size = 1)

#Plot prediction and cnt by datetime
bikesAugust %>%
  mutate(dayOfMonth = (instant - min(instant))/24) %>%
  gather(key = valueType, value = value, pred, cnt) %>%
  filter(dayOfMonth < 14) %>%
  ggplot(aes(x = dayOfMonth, y = value, col = valueType, linetype = valueType)) +
    geom_point() +
    geom_line() +
    scale_x_continuous('Day of Month', breaks = 0:14, labels = 0:14) +
    scale_color_brewer(palette = 'Dark2') +
    ggtitle('Predict vs Actual Bike Rentals, Quasipoisson Model')
