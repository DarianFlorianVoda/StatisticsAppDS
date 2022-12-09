library(klaR)
library(ggord)
library(psych)
library(MASS)
library(devtools)
library(tidyverse)
library(caret)
library(mosaic)

data("iris")
str(iris)


pairs.panels(iris[1:4],
             gap = 0,
             bg = c("red", "green", "blue")[iris$Species],
             pch = 21)


set.seed(123)
ind <- sample(2, nrow(iris),
                replace = TRUE,
                prob = c(0.6, 0.4))
training <- iris[ind==1,]
testing <- iris[ind==2,]

linear <- lda(Species~., training)
linear

p <- predict(linear, training)
names(p)

head(p$class, 6)

head(p$posterior, 6)

head(p$x, 3)


p <- predict(linear, training)
ldahist(data = p$x[,1], g = training$Species)

p <- predict(linear, training)
ldahist(data = p$x[,2], g = training$Species)


lda.data <- cbind(training, predict(linear)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = Species))


ggord(linear, training$Species,
      ylim = c(-10, 10))


partimat(Species~., data = training,
         method = "lda")

p1 <- predict(linear, training)$class
tab <- table(Predicted = p1,
               Actual = training$Species)
tab
sum(diag(tab))/sum(tab)

p2 <- predict(linear, testing)$class
tab1 <- table(Predicted = p2,
                Actual = testing$Species)
tab1
sum(diag(tab1))/sum(tab1)


quadratic <- qda(Species~., data = training)
quadratic

predquad <- predict(quadratic, training)
names(predquad)


pq1 <- predict(quadratic, training)$class
tab <- table(Predicted = pq1,
               Actual = training$Species)
tab
sum(diag(tab))/sum(tab)


pq2 <- predict(quadratic, testing)$class
tab1 <- table(Predicted = pq2,
                Actual = testing$Species)
tab1
sum(diag(tab1))/sum(tab1)


partimat(Species~., data = training,
         method = "qda")

