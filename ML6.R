library(caret)
library(dplyr)
library(mltools)
library(rpart)
library(isotree)
library(plotly)


# import the data PenguinsWithoutMissingValues
setwd("C:/Users/Dari-Laptop/Desktop/FH Karnten - Master - AppDs/StatisticsAppDSLaptop")
data = read.csv("PenguinsWithoutMissingValues.csv")


# encode categorical data
data$Species = factor(data$Species)
data$Island = factor(data$Island)
data$Gender = factor(data$Gender)

# predict the average depth

model <- isolation.forest(data, ndim=1, ntrees=10, nthreads=1)
scores <- predict(model, data, type="avg_depth")
par(mar = c(4,5,3,2))
plot(data$CulmenLength.mm., scores, type="p", col="darkred",
     main="Average isolation depth\nfor CulmenLength(mm)",
     xlab="value", ylab="Average isolation depth")


plot(data$CulmenDepth.mm., scores, type="p", col="darkred",
     main="Average isolation depth\nfor CulmenDepth(mm)",
     xlab="value", ylab="Average isolation depth")

plot(data$FlipperLength.mm., scores, type="p", col="darkred",
     main="Average isolation depth\nfor FlipperLength(mm)",
     xlab="value", ylab="Average isolation depth")

plot(data$BodyMass.g., scores, type="p", col="darkred",
     main="Average isolation depth\nfor BodyMass(g)",
     xlab="value", ylab="Average isolation depth")


# calculate the anomaly score (see slides or original paper)

data$score <- predict(model, newdata = data)

data$score


# use ggplotly scatterplot to visualize the anomaly scores and show the IndividualIDs in the hoover text

p = ggplot(data, aes(x=score, y = 1:nrow(data), color=Species, text = paste("IndividualID :", IndividualID))) + 
  geom_point() + labs(title="Anomaly scores",
                      x ="Score", y = "Number")

ggplotly(p)
