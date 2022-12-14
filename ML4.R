# Delete all variables 
rm( list = ls() )

# Import libraries
library(caret)
library(dplyr)
library(mltools)
library(rpart)
data = read.csv("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/PenguinsWithoutMissingValues.csv")

head(data)


## delete column IndividualID
drop <- c('IndividualID')

df = data[,!(names(data) %in% drop)]

head(df)

## encode Species and Island
df$Species = as.numeric(factor(df$Species))
df$Island = as.numeric(factor(df$Island))

df$Species
df$Island

df

## Use and evaluate the classifier to predict the Gender

trainIndex <- createDataPartition(df$Gender, p = .8, list = FALSE, times = 1)

training

training <- df[ trainIndex,]
testing  <- df[-trainIndex,]

# 10 CV Fold
set.seed(998)
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

# Decision trees model
set.seed(825)
rpartFit1 <- train(Gender ~ ., data = training, 
                 method = "rpart", 
                 trControl = fitControl)
rpartFit1

dim(training)

plot(rpartFit1$finalModel, uniform=TRUE,
     main="Classification Tree")
text(rpartFit1$finalModel, use.n.=TRUE, all=TRUE, cex=.8)

## predict test set

gender.pred = predict(rpartFit1, newdata = testing)
table(gender.pred, testing$Gender)

error.rate = round(mean(gender.pred != testing$Gender),2)
error.rate

gender.pred


## predict the gender of PenguinsWithoutGender

data2 = read.csv("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/DataWithoutGender.csv")

## delete column IndividualID
drop <- c('IndividualID')

df2 = data2[,!(names(data) %in% drop)]

head(df2)

## encode Species and Island
df2$Species = as.numeric(factor(df2$Species))
df2$Island = as.numeric(factor(df2$Island))

df2$Species
df2$Island

df2


## Use and evaluate the classifier to predict the Gender

trainIndex2 <- createDataPartition(df$Gender, p = .8, list = FALSE, times = 1)

training2 <- df[ trainIndex2,]
testing2  <- df2[-trainIndex2,]

# 10 CV Fold
set.seed(998)
fitControl2 <- trainControl(
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

# Decision trees model
set.seed(825)
rpartFit2 <- train(Gender ~ ., data = training, 
                   method = "rpart", 
                   trControl = fitControl2)
rpartFit2

dim(training2)

plot(rpartFit2$finalModel, uniform=TRUE,
     main="Classification Tree")
text(rpartFit2$finalModel, use.n.=TRUE, all=TRUE, cex=.8)


## predict test set

df2$Gender

testing2$Gender

gender.pred2 = predict(rpartFit2, newdata = testing2)

gender.pred2



####################

# GBM training method
set.seed(825)
gbmFit1 <- train(Gender ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1

rpartFit1

plot(gbmFit1, metric = "Kappa", plotType = "level",
     scales = list(x = list(rot = 90)))

plot(gbmFit1)


gender.pred3 = predict(gbmFit1, newdata = testing)
table(gender.pred3, testing$Gender)

error.rate = round(mean(gender.pred3 != testing$Gender),2)
error.rate


#### Isolation Forests

