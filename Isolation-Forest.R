# required libraries
library(tidyverse)
library(dplyr)
library(solitude)
library(ggplot2)

##=== The code has the following steps:
# 1. Generate data that includes anomalies
# 2. Plot the data to view how the anomalies plot against the normal observations
# 3. Fit an isolation forest on test set and obtain anomaly scores
# 4. Predict on a newly generated test set

##=== Generate train data
set.seed(711)
# generates anomalies from a uniform distribution
anomalies = runif(n = 50, min = -200, max = 420)
y_anomalies = runif(n = 50, min = -130, max = 300)
errors = data.frame(x = anomalies, y = y_anomalies)

# generates sets of normal observations from a normal distribution
x = rnorm(n = 200, mean = 5, sd = 20)
y = rnorm(n = 200, mean = 25, sd = 30)
x2 = rnorm(n = 200, mean = 200, sd = 30)
y2 = rnorm(n = 200, mean = 200, sd = 20)
x3 = rnorm(n = 200, mean = 400, sd = 20)
y3 = rnorm(n = 200, mean = 2, sd = 20)
df = data.frame(x,y)
df2 = data.frame(x = x2, y = y2)
df3 = data.frame(x = x3, y = y3)

# dataframe containing the normal observations and the anomalies
if_df = rbind(df, errors, df2, df3)

# plot the data to view normal points and the anomalies
ggplot() + geom_point(data = df, mapping = aes(x,y), col = "blue", alpha = 0.4) + 
  geom_point(data = errors, mapping = aes(x, y), col = "red", alpha = 0.7) + 
  geom_point(data = df2, mapping = aes(x, y), col = "green", alpha = 0.4) +
  geom_point(data = df3, mapping = aes(x, y), col = "purple", alpha = 0.4) 

# run the isolationForest algorithm
iforest = isolationForest$new() # initiates a solitude object
iforest$fit(if_df) # fits an isolation forest for the dataframe
score = iforest$scores # provides the anomaly scores for each observation in the data set
plot(density(score$anomaly_score)) # density plot

# Note: scores closer to 1 may be anomalous

##=== Generate test data
set.seed(711)
# generates anomalies from a uniform distribution
anomalies.test = runif(n = 25, min = -200, max = 200)
y_anomalies.test = runif(n = 25, min= -200, max = 200)
errors.test = data.frame(x = anomalies.test, y = y_anomalies.test)

# generates sets of normal observations from a normal distribution
x.test = rnorm(n = 2500, mean = 0, sd = 20)
y.test = rnorm(n = 2500, mean = 0, sd = 20)
df.test = data.frame(x = x.test,y = y.test)

# dataframe containing the normal observations and the anomalies
if_df.test = rbind(df.test, errors.test)

# plot the data to view normal points and the anomalies
ggplot() + geom_point(data = df.test, mapping = aes(x,y), col = "blue", alpha = 0.4) + 
  geom_point(data = errors.test, mapping = aes(x, y), col = "red", alpha = 0.7)

preds = iforest$predict(if_df.test)

indices = preds[which(preds$anomaly_score > 0.53)]

pred_anomalies = if_df.test[indices$id, ]
pred_normal = if_df.test[-indices$id, ]

ggplot() + geom_point(data = pred_normal, mapping = aes(x,y), col = "blue", alpha = 0.4) + 
  geom_point(data = pred_anomalies, mapping = aes(x, y), col = "red", alpha = 0.7)
