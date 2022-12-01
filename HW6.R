library(dplyr)
library(ggplot2)
library(car)
library(tidyverse)
library(ggpubr)
library(rstatix)



#### Exercise 70 ####

data<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/fastfood.txt",
                stringsAsFactors=F)

# EDA
head(data)

# EDA
group_by(data, Menu) %>%
  summarise(
    count = n(),
    mean = mean(Sales, na.rm = TRUE),
    sd = sd(Sales, na.rm = TRUE)
    )

# Boxplot
ggplot(data, aes(x=Menu, y=Sales, fill=Menu)) + 
  geom_boxplot()+
  labs(title="Plot of Sales per Menu",x="Dose (mg)", y = "Length")+
  theme(plot.title = element_text(hjust = 0.5))

# Levene's Test for homogeneity
leveneTest(Sales ~ Menu, data = data)

# Density plot
ggdensity(data$Sales, fill = "lightblue") + labs(x="Sales")

# QQ plot
ggqqplot(data$Sales) + labs(x="Sales")

# ANOVA test
res.aov <- aov(Sales ~ Menu, data = data)

# Anova variable
res.aov

# Extracting the p-value and F value
summary(res.aov)

# QQ plot of ANOVA
plot(res.aov, 2)

# Get rid of outliers
aov_residuals <- residuals(object = res.aov)

# Normality test using Shapiro-Wilkins test
shapiro.test(x = aov_residuals)


#### Exercise 71 ####

ICM<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/ICM.txt",
                stringsAsFactors=F)

# EDA
head(ICM)

# EDA
group_by(ICM, Socialmediahours) %>%
  summarise(
    count = n(),
    mean = mean(NegativeMood, na.rm = TRUE),
    sd = sd(NegativeMood, na.rm = TRUE)
  )

# Boxplot
ggplot(ICM, aes(x=Socialmediahours, y=NegativeMood, fill=Socialmediahours)) + 
  geom_boxplot()+
  labs(title="Plot of NegativeMood per Socialmediahours",x="Social Media (hours/day)", y = "Negative Mood")+
  theme(plot.title = element_text(hjust = 0.5))

# Levene's Test for homogeneity
leveneTest(NegativeMood ~ Socialmediahours, data = ICM)

# Density plot
ggdensity(ICM$NegativeMood, fill = "lightblue") + labs(x="NegativeMood")

# QQ plot
ggqqplot(ICM$NegativeMood) + labs(x="NegativeMood")

# ANOVA test
res.aov <- aov(NegativeMood ~ Socialmediahours, data = ICM)

# Anova variable
res.aov

# Extracting the p-value and F value
summary(res.aov)

# QQ plot of ANOVA
plot(res.aov, 2)

# Get rid of outliers
aov_residuals <- residuals(object = res.aov)

# Normality test using Shapiro-Wilkins test
shapiro.test(x = aov_residuals)


#### Exercise 72 ####

ICM<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/ICM.txt",
                stringsAsFactors=F)

# EDA
head(ICM)

# EDA
group_by(ICM, Timewithfriends) %>%
  summarise(
    count = n(),
    mean = mean(Socialization, na.rm = TRUE),
    sd = sd(Socialization, na.rm = TRUE)
  )

# Boxplot
ggplot(ICM, aes(x=Timewithfriends, y=Socialization, fill=Timewithfriends)) + 
  geom_boxplot()+
  labs(title="Plot of Socialization per Timewithfriends",x="Time with friends (hours/week)", 
       y = "Socialization")+
  theme(plot.title = element_text(hjust = 0.5))

# Levene's Test for homogeneity
leveneTest(Socialization ~ Timewithfriends, data = ICM)

# Density plot
ggdensity(ICM$Socialization, fill = "lightblue") + labs(x="Socialization")

# QQ plot
ggqqplot(ICM$Socialization) + labs(x="Socialization")

# ANOVA test
res.aov <- aov(Socialization ~ Timewithfriends, data = ICM)

# Anova variable
res.aov

# Extracting the p-value and F value
summary(res.aov)

# QQ plot of ANOVA
plot(res.aov, 2)

# Get rid of outliers
aov_residuals <- residuals(object = res.aov)

# Normality test using Shapiro-Wilkins test
shapiro.test(x = aov_residuals)


#### Exercise 75 ####
head(mtcars)

# linear model
lm(mpg ~ wt, data=mtcars)


mpg.lm = lm(mpg ~ wt, data=mtcars)

summary(mpg.lm)

coeffs = coefficients(mpg.lm)
coeffs

# weight (in 1000 lbs)
weight.auto = 3.00
duration = coeffs[1] + coeffs[2]*weight.auto

# mpg with respect to weight=3
duration

paste("Based on the simple linear regression model, 
      if the weight of the cars has been 3.00 (in 1000 lbs), 
      we expect to consume 1 gallon of gas at 21.25 miles")


# Plot
plot(mtcars$wt, mtcars$mpg, xlab="Weight", ylab="Miles per gallon")
abline(lm(mtcars$mpg ~ mtcars$wt))

# Coefficient determination
mpg.lm = lm(mpg ~ wt, data=mtcars)
summary(mpg.lm)$r.squared

paste("The results suggests that 75% of the dependent variable is predicted by the independent variable")

# Significant relationship between variables
mpg.lm = lm(mpg ~ wt, data=mtcars)
summary(mpg.lm)

paste("As the p-value is 0.000000000129, 
      which is much less than 0.05, 
      we reject the null hypothesis that beta = 0.")

# Confidence Interval for weight = 3
mpg.lm = lm(mpg ~ wt, data=mtcars)
newdata=data.frame(wt=3.00)
predict(mpg.lm, newdata, interval="confidence")


paste("The 95% confidence interval of the mean miles per gallon for the weight of 3.00,
      is between 20.12444 and 22.37899 miles per gallon.")


# Residual Plot
mpg.lm = lm(mpg ~ wt, data=mtcars)
mpg.res=resid(mpg.lm)
plot(mtcars$wt, mpg.res, ylab="Residuals", xlab="Weight",
     main="Mtcars Weights (in 1000 lbs)", col="blue")
abline(0, 0)

# Normal Probability Plot of Residuals
mpg.lm = lm(mpg ~ wt, data=mtcars)
mpg.stdres = rstandard(mpg.lm)
qqnorm(mpg.stdres, ylab="Standardized Residuals", xlab="Normal Scores", main="Mtcars data", col="red")
qqline(mpg.stdres)


#### Exercise 76 ####
incomehappy<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/incomehappy.txt",
                stringsAsFactors=F)


head(incomehappy)

# linear model
lm(happiness ~ income, data=incomehappy)


happiness.lm = lm(happiness ~ income, data=incomehappy)

summary(happiness.lm)

coeffs = coefficients(happiness.lm)
coeffs

# income
income.auto = 6.00
duration = coeffs[1] + coeffs[2]*income.auto

# happiness with respect to income=3
duration

paste("Based on the simple linear regression model, 
      if the income has been 6.00 (in 1000 Euro), 
      we expect to have a happiness index of 4.487223")


# Plot
plot(incomehappy$income, incomehappy$happiness, xlab="Income", ylab="Happiness")
abline(lm(incomehappy$happiness ~ incomehappy$income))

# Coefficient determination
happiness.lm = lm(happiness ~ income, data=incomehappy)
summary(happiness.lm)$r.squared

paste("The results suggests that 74% of the dependent variable is predicted by the independent variable")

# Significant relationship between variables
happiness.lm = lm(happiness ~ income, data=incomehappy)
summary(happiness.lm)

paste("As the p-value is very very low, 
      which is much less than 0.05, 
      we reject the null hypothesis that beta = 0.")

# Confidence Interval for income = 6
happiness.lm = lm(happiness ~ income, data=incomehappy)
newdata=data.frame(income=6.00)
predict(happiness.lm, newdata, interval="confidence")

paste("The 95% confidence interval of the mean happiness index for the income of 6.00,
      is between 4.40287 and 4.571577.")


# Residual Plot
happiness.lm = lm(happiness ~ income, data=incomehappy)
happiness.res=resid(happiness.lm)
plot(incomehappy$income, happiness.res, ylab="Residuals", xlab="Weight",
     main="Happiness by Income", col="blue")
abline(0, 0)

# Normal Probability Plot of Residuals
happiness.lm = lm(happiness ~ income, data=incomehappy)
happiness.stdres = rstandard(happiness.lm)
qqnorm(happiness.stdres, ylab="Standardized Residuals", xlab="Normal Scores", main="incomehappy data", col="red")
qqline(happiness.stdres)


#### Exercise 79 ####
students<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/students.txt",
                        stringsAsFactors=F)


head(students)

# Computation of the correlation coefficient
cor(students$Weight_kg, students$Size_cm)

# Simple plot + Scatter plot
plot(students$Weight_kg, students$Size_cm, xlab="Weight (kg)", ylab="Height (cm)")
ggscatter(students, x = "Weight_kg", y = "Size_cm",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Weight (kg)", ylab = "Height (cm)")

# Shapiro-Wilk normality tests
shapiro.test(students$Weight_kg)
shapiro.test(students$Size_cm)

paste("Both p-values < 0.05, but, however, our sample is 82, so data is normally distributed (CLT)")

# QQ Plots of the variables
ggqqplot(students$Weight_kg, ylab = "Weight (kg)")
ggqqplot(faithful$waiting, ylab = "Height (cm)")

# Significance level (p-value) of the correlation
cor.test(students$Weight_kg, students$Size_cm,
         method = "pearson")

#### Exercise 80 ####
ICM<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/ICM.txt",
                stringsAsFactors=F)


head(ICM)

# Computation of the correlation coefficient
cor(ICM$NegativeMood, ICM$PositiveMood, use="complete.obs")

# Simple plot + Scatter plot
plot(ICM$NegativeMood, ICM$PositiveMood, xlab="Negative Mood", ylab="Positive Mood")
ggscatter(ICM, x = "NegativeMood", y = "PositiveMood",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Negative Mood", ylab = "Positive Mood")

# Shapiro-Wilk normality tests
shapiro.test(ICM$NegativeMood)
shapiro.test(ICM$PositiveMood)

paste("Both p-values < 0.05, but, however, our sample is 199, so data is normally distributed (CLT)")

# QQ Plots of the variables
ggqqplot(ICM$NegativeMood, ylab = "Negative Mood")
ggqqplot(ICM$PositiveMood, ylab = "Positive Mood")

# Significance level (p-value) of the correlation
cor.test(ICM$NegativeMood, ICM$PositiveMood,
         method = "pearson")

#### Exercise 83 ####
students<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/students.txt",
                     stringsAsFactors=F)

# Significance level (p-value) of the correlation
cor.test(students$Weight_kg, students$Size_cm,
         method = "spearman", exact=FALSE)

#### Exercise 84 ####

# Significance level (p-value) of the correlation
ICM<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/ICM.txt",
                stringsAsFactors=F)


head(ICM)

# Computation of the correlation coefficient
cor(ICM$NegativeMood, ICM$OHS, use="complete.obs")

# Simple plot + Scatter plot
plot(ICM$NegativeMood, ICM$OHS, xlab="Negative Mood", ylab="OHS")
ggscatter(ICM, x = "NegativeMood", y = "OHS",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Negative Mood", ylab = "OHS")

# Shapiro-Wilk normality tests
shapiro.test(ICM$NegativeMood)
shapiro.test(ICM$OHS)

paste("Both p-values < 0.05, but, however, our sample is 199, so data is normally distributed (CLT)")

# QQ Plots of the variables
ggqqplot(ICM$NegativeMood, ylab = "Negative Mood")
ggqqplot(ICM$OHS, ylab = "OHS")

# Significance level (p-value) of the correlation
cor.test(ICM$NegativeMood, ICM$OHS,
         method = "spearman", exact=FALSE)
