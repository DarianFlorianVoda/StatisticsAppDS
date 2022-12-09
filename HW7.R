library(ggplot2)
library(klaR)
library(ggord)
library(psych)
library(MASS)
library(devtools)
library(tidyverse)
library(caret)
library(mosaic)
library(broom)


# Without standardization
data<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/Discriminant-pulmonary.txt",
                stringsAsFactors=F)
# Look at the data
head(data)

# Get type of the data
str(data)

pairs.panels(data[2:6],
             gap = 0,
             bg = c("red", "green")[data$outcome],
             pch = 21)

# 70% for training, 30% for testing
set.seed(123)
ind <- sample(2, nrow(data),
                replace = TRUE,
                prob = c(0.7, 0.3))
training <- data[ind==1,]
testing <- data[ind==2,]

linear <- lda(outcome~., training)
linear

plot(linear)

paste("Prior probabilities of survival: 50%/50% rate of survival")

# prediction
p <- predict(linear, training)
names(p)

# Predicted classes
head(p$class, 6)

# Predicted probabilities
head(p$posterior, 6)

# Linear discriminants
head(p$x, 3)

# Stacked histogram for LD1
p <- predict(linear, training)
ldahist(data = p$x[,1], g = training$outcome)

lda.data <- cbind(training, predict(linear)$x)
ggplot(lda.data, aes(LD1, age)) +
  geom_point(aes(color = outcome)) +
  labs(title="Outcome of patients with pulmonary problems")

# plot the classes using density plot
ggplot(data = lda.data)+
  geom_density(aes(age, fill = outcome), alpha = 0.2)+
  scale_fill_discrete(name = "Outcome of patients", labels = c("Dead", "Survived"))

partimat(factor(outcome)~., data = training,
         method = "lda")

# prediction accuracy of training set
p1 <- predict(linear, training)$class
tab <- table(Predicted = p1,
               Actual = training$outcome)
tab
sum(diag(tab))/sum(tab)


# prediction accuracy of test set
p2 <- predict(linear, testing)$class
tab1 <- table(Predicted = p2,
                Actual = testing$outcome)
tab1
sum(diag(tab1))/sum(tab1)

# QDA
quadratic <- qda(outcome~., data = training)
quadratic


predquad <- predict(quadratic, training)
names(predquad)

# prediction accuracy of training set
pq1 <- predict(quadratic, training)$class
tab <- table(Predicted = pq1,
               Actual = training$outcome)
tab
sum(diag(tab))/sum(tab)

# prediction accuracy of test set
pq2 <- predict(quadratic, testing)$class
tab1 <- table(Predicted = pq2,
                Actual = testing$outcome)
tab1
sum(diag(tab1))/sum(tab1)

partimat(factor(outcome)~., data = training,
         method = "qda")

### data with standardization
data2<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/Discriminant-pulmonary.txt",
                 stringsAsFactors=F)

data2_standardized = data2 %>% mutate_at(c('oxygen.conc', 'body.size', 'ventilation.aggressiveness', 'ventilation.time', 'age'),
                         ~(scale(.) %>% as.vector))

head(data2_standardized)


pairs.panels(data2_standardized[2:6],
             gap = 0,
             bg = c("red", "green")[data2_standardized$outcome],
             pch = 21)

# 70% for training, 30% for testing
set.seed(123)
ind <- sample(2, nrow(data2_standardized),
              replace = TRUE,
              prob = c(0.7, 0.3))
training <- data2_standardized[ind==1,]
testing <- data2_standardized[ind==2,]

linear <- lda(outcome~., training)
linear

plot(linear)

paste("Prior probabilities of survival: 50%/50% rate of survival")

# prediction
p <- predict(linear, training)
names(p)

# Predicted classes
head(p$class, 6)

# Predicted probabilities
head(p$posterior, 6)

# Linear discriminants
head(p$x, 3)

# Stacked histogram for LD1
p <- predict(linear, training)
ldahist(data = p$x[,1], g = training$outcome)

lda.data <- cbind(training, predict(linear)$x)
ggplot(lda.data, aes(LD1, age)) +
  geom_point(aes(color = outcome)) +
  labs(title="Outcome of patients with pulmonary problems")

# plot the classes using density plot
ggplot(data = lda.data)+
  geom_density(aes(age, fill = outcome), alpha = 0.2)+
  scale_fill_discrete(name = "Outcome of patients", labels = c("Dead", "Survived"))

partimat(factor(outcome)~., data = training,
         method = "lda")

# prediction accuracy of training set
p1 <- predict(linear, training)$class
tab <- table(Predicted = p1,
             Actual = training$outcome)
tab
sum(diag(tab))/sum(tab)


# prediction accuracy of test set
p2 <- predict(linear, testing)$class
tab1 <- table(Predicted = p2,
              Actual = testing$outcome)
tab1
sum(diag(tab1))/sum(tab1)

# QDA
quadratic <- qda(outcome~., data = training)
quadratic


predquad <- predict(quadratic, training)
names(predquad)

# prediction accuracy of training set
pq1 <- predict(quadratic, training)$class
tab <- table(Predicted = pq1,
             Actual = training$outcome)
tab
sum(diag(tab))/sum(tab)

# prediction accuracy of test set
pq2 <- predict(quadratic, testing)$class
tab1 <- table(Predicted = pq2,
              Actual = testing$outcome)
tab1
sum(diag(tab1))/sum(tab1)

partimat(factor(outcome)~., data = training,
         method = "qda")


#### Exercise 86 ####
data3<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/Discriminant-birds.txt",
                  stringsAsFactors=F)

# Look at the data
head(data3)

# Get type of the data
str(data3)

pairs.panels(data3[2:6],
             gap = 0,
             bg = c("red", "green")[data3$ï..sex],
             pch = 21)

# 70% for training, 30% for testing
set.seed(123)
ind <- sample(2, nrow(data3),
              replace = TRUE,
              prob = c(0.7, 0.3))
training <- data3[ind==1,]
testing <- data3[ind==2,]

linear <- lda(ï..sex~., training)
linear

paste("Prior probabilities of sex: 32% female, 15% male, 52% unknown")

# prediction
p <- predict(linear, training)
names(p)

# Predicted classes
head(p$class, 6)

# Predicted probabilities
head(p$posterior, 6)

# Linear discriminants
head(p$x, 3)

# Stacked histogram for LD1
p <- predict(linear, training)
ldahist(data = p$x[,1], g = training$ï..sex)

lda.data <- cbind(training, predict(linear)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = ï..sex)) +
  labs(title="Sex of birds")

# Biplot
ggord(linear, training$ï..sex,
      ylim = c(-10, 10), xlim=c(-10,10))

partimat(factor(ï..sex)~., data = training,
         method = "lda")

# prediction accuracy of training set
p1 <- predict(linear, training)$class
tab <- table(Predicted = p1,
             Actual = training$ï..sex)
tab
sum(diag(tab))/sum(tab)


# prediction accuracy of test set
p2 <- predict(linear, testing)$class
tab1 <- table(Predicted = p2,
              Actual = testing$ï..sex)
tab1
sum(diag(tab1))/sum(tab1)

# QDA
quadratic <- qda(ï..sex~., data = training)
quadratic


predquad <- predict(quadratic, training)
names(predquad)

# prediction accuracy of training set
pq1 <- predict(quadratic, training)$class
tab <- table(Predicted = pq1,
             Actual = training$ï..sex)
tab
sum(diag(tab))/sum(tab)

# prediction accuracy of test set
pq2 <- predict(quadratic, testing)$class
tab1 <- table(Predicted = pq2,
              Actual = testing$ï..sex)
tab1
sum(diag(tab1))/sum(tab1)

partimat(factor(ï..sex)~., data = training,
         method = "qda")
