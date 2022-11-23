library(mosaic)
library(tidyverse)
library(hrbrthemes)
library(ggpubr)
library(dplyr)
library(ggplot2)
library(viridis)
library(gridExtra)
library(car)
library(graphics)
library(MASS)


#### Exercise 53 ####

ICM<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/ICM.txt",
                stringsAsFactors=F)


head(ICM,5)

inspect(ICM, digits=3)


krus.res <- kruskal.test(NegativeMood ~ Socialmediahours, data =
                           ICM)
krus.res

paste("p-value is 0.007 which is less than 0.05, thus we reject H0")

ggplot(ICM, aes(group=Socialmediahours, y=NegativeMood, color=Socialmediahours)) +
  geom_boxplot()+
  labs(title="Negative mood and Social media use distribution")+
  theme(plot.title = element_text(hjust = 0.5))


#### Exercise 54 ####
ICM<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/ICM.txt",
                stringsAsFactors=F)

tail(ICM)


krus.res <- kruskal.test(Socialization ~ Timewithfriends, data =
                           ICM)
krus.res

paste("p-value is 0.00001198 which is less than 0.05, thus we reject H0")

ggplot(ICM, aes(x=Timewithfriends, y=Socialization, color=Timewithfriends)) +
  geom_violin(width=0.9)+
  geom_boxplot(width=0.3)+
  scale_fill_viridis(discrete = TRUE, alpha=0.6)+
  labs(title="Time with friends and Socialization distribution")+
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")



#### Exercise 56 #####

survey<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/survey_PCA.txt",
                   stringsAsFactors=F)

qqplot <- ggplot(survey, aes(sample = openness)) + geom_qq_line() + stat_qq(color="red") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  labs(title="Distribution of openness")

densityplot <- ggplot(survey, aes(openness)) + geom_density(color="darkblue", fill="lightblue") + 
  geom_vline(aes(xintercept=mean(openness)), color="red", linetype="dashed", size=1)
bxplot <- ggplot(survey, aes(openness)) + geom_boxplot(color="red", fill="orange")
grid.arrange(qqplot, densityplot, bxplot, ncol = 1)


shapiro.test(survey$openness)

paste("p-value is greater than 0.5, so H0 is accepted")


#### Exercise 57 #####

survey<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/survey_PCA.txt",
                   stringsAsFactors=F)

qqplot <- ggplot(survey, aes(sample = compatibility)) + geom_qq_line() + stat_qq(color="red") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  labs(title="Distribution of compatibility")

densityplot <- ggplot(survey, aes(compatibility)) + geom_density(color="darkblue", fill="lightblue") + 
  geom_vline(aes(xintercept=mean(compatibility)), color="red", linetype="dashed", size=1)
bxplot <- ggplot(survey, aes(compatibility)) + geom_boxplot(color="red", fill="orange")
grid.arrange(qqplot, densityplot, bxplot, ncol = 1)


shapiro.test(survey$compatibility)

paste("p-value is less than 0.5, so H0 is rejected")


#### Exercise 58 ####

survey<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/survey_PCA.txt",
                   stringsAsFactors=F)

qqplot <- ggplot(survey, aes(sample = conscientiousness)) + geom_qq_line() + stat_qq(color="red") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  labs(title="Distribution of conscientiousness")

densityplot <- ggplot(survey, aes(conscientiousness)) + geom_density(color="darkblue", fill="lightblue") + 
  geom_vline(aes(xintercept=mean(conscientiousness)), color="red", linetype="dashed", size=1)
bxplot <- ggplot(survey, aes(conscientiousness)) + geom_boxplot(color="red", fill="orange")
grid.arrange(qqplot, densityplot, bxplot, ncol = 1)


shapiro.test(survey$conscientiousness)

paste("p-value is less than 0.5, so H0 is rejected")


#### Exercise 60 ####

ICM<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/ICM.txt",
                stringsAsFactors=F)


box = ggplot(ICM, aes(y=OHS)) +
  geom_boxplot(width=0.3, color="blue", fill="lightblue")+
  scale_fill_viridis(discrete = TRUE, alpha=0.6)+
  labs(title="Oxford Happiness Score")+
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")


1 - pt(1:5, df = ICM$OHS)
qt(.975, df = ICM$OHS)
tt <- seq(0, 10, length.out = 21)
ncp <- seq(0, 6, length.out = 31)
ptn <- outer(tt, ncp, function(t, d) pt(t, df = ICM$OHS, ncp = d))
t.tit <- "Oxford Happines Score"
matrixx = image(tt, ncp, ptn, zlim = c(0,1), main = t.tit)

hist = ggplot(ICM, aes(x=OHS)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="lightblue")+
  geom_density(alpha=.2, fill="red")+
  labs(title="Oxford Happiness Score")+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(box, hist, ncol=2)


res <- t.test(ICM$OHS, mu = 4)
res

paste("p-value less than 0.05, thus it is rejected")

#### Exercise 61 ####

data = read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/height.txt",
                  stringsAsFactors=F)


box = ggplot(data, aes(y=Height)) +
  geom_boxplot(width=0.3, color="blue", fill="lightblue")+
  scale_fill_viridis(discrete = TRUE, alpha=0.6)+
  labs(title="Height (Inches)")+
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")


hist = ggplot(data, aes(x=Height)) + 
  geom_histogram(aes(y=..density..), colour="black", binwidth=1, fill="lightblue")+
  geom_density(alpha=0.3, fill="red")+
  geom_vline(xintercept=66.5, color="orange", size=1)+
  geom_text(label="US Mean", aes(x=68.7, y=0.1), angle=45, size=4)+
  labs(title="Height (Inches)")+
  theme(plot.title = element_text(hjust = 0.5))

  
grid.arrange(box, hist, ncol=2)


res <- t.test(data$Height, mu = 66.5)
res

paste("p-value is 0.00000001264 which is less than 0.05, thus H0 is rejected")


#### Exercise 63 ####

data = read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/diet_paired.txt",
                  stringsAsFactors=F)

head(data)

summary(data)


data_transformed <- data.frame(
  diet = c(data$before_diet, data$after_diet),
  time = c(
    rep("before", length(data$before_diet)),
    rep("after", length(data$after_diet))))

ggplot(data_transformed, aes(x = time, y = diet,
                              color = time)) + geom_boxplot() + labs(title="Body weight regarding to Diet")+
  theme(plot.title = element_text(hjust = 0.5), legend.position="top")


ggplot(data_transformed, aes(diet, color = time)) +
  geom_density(fill="lightblue", alpha=0.2) + annotate("segment",
                              x = mean(data$before_diet),
                              xend = mean(data$before_diet),
                              y = 0, yend = 0.055, color = "black") +
  annotate("text",
             x = mean(data$before_diet),
             y = 0.057,
             label = expression(mu[before])) +
  annotate("segment",
              x = mean(data$after),
              xend = mean(data$after_diet),
              y = 0, yend = 0.055, color = "black") +
  annotate("text",
              x = mean(data$after_diet),
              y = 0.057,
              label = expression(mu[after])) + labs(title="Body weight regarding to Diet")+
  theme(plot.title = element_text(hjust = 0.5))

result <-t.test(data$before_diet, data$after_diet, paired=TRUE)
result

paste("p-value is 0.03124 which is less than 0.05, thus H0 is rejected")

#### Exercise 64 ####

data = read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/OHS_2020_paired.txt",
                  stringsAsFactors=F)

head(data)

summary(data)


data_transformed <- data.frame(
  points = c(data$OHS_1, data$OHS_2, data$OHS_3),
  time = c(
    rep("OHS1", length(data$OHS_1)),
    rep("OHS2", length(data$OHS_2)),
    rep("OHS3", length(data$OHS_3))))

ggplot(data_transformed, aes(x = time, y = points,
                             color = time)) + geom_boxplot() + labs(title="OHS Score regarding to time points")+
  theme(plot.title = element_text(hjust = 0.5), legend.position="top")


ggplot(data_transformed, aes(points, color = time)) +
  geom_density(fill="steelblue", alpha=0.2) + annotate("segment",
                                                       x = mean(data$OHS_1, na.rm=TRUE),
                                                       xend = mean(data$OHS_1, na.rm=TRUE),
                                                       y = 0, yend = 0.8, color = "black") +
  annotate("text",
           x = mean(data$OHS_1, na.rm=TRUE),
           y = 0.81,
           label = expression(mu[OH1])) +
  annotate("segment",
           x = mean(data$OHS_2),
           xend = mean(data$OHS_2),
           y = 0, yend = 0.81, color = "black") +
  annotate("text",
           x = mean(data$OHS_2),
           y = 0.83,
           label = expression(mu[OH2])) +
  annotate("segment",
           x = mean(data$OHS_3, na.rm=TRUE),
           xend = mean(data$OHS_3, na.rm=TRUE),
           y = 0, yend = 0.82, color = "black") +
  annotate("text",
           x = mean(data$OHS_3, na.rm=TRUE),
           y = 0.85,
           label = expression(mu[OH3])) +labs(title="OHS Score regarding to time points")+
  theme(plot.title = element_text(hjust = 0.5))

result1 <-t.test(data$OHS_1, data$OHS_2, paired=TRUE)
result1

paste("p-value for OHS_1 and OHS_2 is 0.08281 which is greather than 0.05, thus H0 is accepted")

result2 <-t.test(data$OHS_1, data$OHS_3, paired=TRUE)
result2

paste("p-value for OHS_1 and OHS_3 is 0.07092 which is greather than 0.05, thus H0 is accepted")

result3 <-t.test(data$OHS_2, data$OHS_3, paired=TRUE)
result3

paste("p-value for OHS_2 and OHS_3 is 0.3185 which is greather than 0.05, thus H0 is accepted")

#### Exercise 66 ####
ICM<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/ICM.txt",
                stringsAsFactors=F)

head(ICM)

ggplot(ICM, aes(group = Gender, y = OHS, fill=Gender))+ geom_boxplot(alpha=1)

result_t_test <- t.test(OHS ~ Gender, data=ICM)
result_t_test

paste("p-value is 0.7284 which is greater than 0.05, thus H0 is accepted")

mean_female <- mean(ICM$OHS[ICM$Gender == "female"],
                       na.rm = T)
mean_female
mean_male <- mean(ICM$OHS[ICM$Gender == "male"],
                  na.rm = T)
mean_male


#### Exercise 67 ####

ICM<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/ICM.txt",
                stringsAsFactors=F)

head(ICM)

ggplot(ICM, aes(group = Siblings, y = Communication_open_direct, fill=Siblings)) + 
  geom_boxplot(alpha=1) +
  labs(title="Communication (open and direct) between people with/without siblings")+
  theme(plot.title = element_text(hjust = 0.5))

result_t_test <- t.test(Communication_open_direct ~ Siblings, data=ICM)
result_t_test

paste("p-value is 0.09877 which is greater than 0.05, thus H0 is accepted")

mean_siblings <- mean(ICM$Communication_open_direct[ICM$Siblings == "Yes"],
                    na.rm = T)
mean_siblings
mean_no_siblings <- mean(ICM$Communication_open_direct[ICM$Siblings == "No"],
                  na.rm = T)
mean_no_siblings


#### Exercise 68 ####

ICM<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/ICM.txt",
                stringsAsFactors=F)

head(ICM)

ggplot(ICM, aes(group = Children, y = Mentalhealth, fill=Children)) + 
  geom_boxplot(alpha=1) +
  labs(title="Mental health distribution between people with/without children")+
  theme(plot.title = element_text(hjust = 0.5))

result_t_test <- t.test(Mentalhealth ~ Children, data=ICM)
result_t_test

paste("p-value is 0.02925 which is less than 0.05, thus H0 is rejected")

mean_children <- mean(ICM$Mentalhealth[ICM$Children == "Yes"],
                      na.rm = T)
mean_children
mean_no_children <- mean(ICM$Mentalhealth[ICM$Children == "No"],
                         na.rm = T)
mean_no_children
