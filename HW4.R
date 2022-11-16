####  Exercise 33 ####
library(mosaic)
library(ggplot2)
library(latex2exp)
library(gridExtra)
library(knitr)
library(gridExtra)
library(ggmosaic)
library(MASS)
library(cowplot)
library(PairedData)

data <- matrix(c(41, 21, 55, 1000 - 41, 1000 - 21, 1000 - 55), ncol=2)
dimnames(data) <- list(
  producer = c("Producer A", "Producer B", "Producer C"),
  produced = c("errors", "not errors"))
kable(data)

mosaicplot(data, col=c(7,5))

errors_producer <- sum(data[, "errors"]) / sum(data)
errors_producer

not_errors_producer <- sum(data[, "not errors"]) /
  sum(data)
not_errors_producer

errors_producer + not_errors_producer


expected <- c(
  (data[, "errors"] + data[, "not errors"]) * errors_producer,
  (data[, "errors"] + data[, "not errors"]) * not_errors_producer)

expected

table_expected <- matrix(expected, ncol=2)
dimnames(table_expected) <- list(
  producer = c("Producer A", "Producer B", "Producer C"),
  produced = c("errors", "not errors"))
kable(table_expected)

x <- sum((data - expected)^2 / expected)
x

alpha = 0.01
df = 2
crit_val <- qchisq(alpha, df, lower.tail = FALSE)
crit_val

plot_chisq <- function(sim_data, x, crit_val, title = ""
) {
  annotation_y_val <- max(sim_data$y) * 1/3
  plt <- ggplot(sim_data, aes(x, y)) + ggtitle(title) +
    labs(y = "density") + geom_line() + geom_area(
      data = subset(sim_data, x >= crit_val),
      fill = "red", alpha = 0.24) +
    annotate("segment", x = crit_val, xend = crit_val,
               y = 0, yend = annotation_y_val,
               color = "red") + annotate("text",
                                           x = crit_val, y = annotation_y_val * 1.3,
                                           label = "chi-critical", parse = TRUE) +
    annotate("segment", x = x, xend = x,
                y = 0, yend = annotation_y_val,
                color = "black") + annotate("text",
                                               x = x, y = annotation_y_val * 1.3, label = "chi-observed", parse = TRUE)
  return(plt)}

sim_data <- data.frame(density(rchisq(1000, df))[c("x", "y")])
plot_chisq(sim_data, x, crit_val)


p_value <- 1 - pchisq(x, df)
p_value


#### Exercise 34 ####
data <- matrix(c(6, 17, 10, 125 - 6, 200 - 17, 325 - 10), ncol=2)
dimnames(data) <- list(
  producer = c("Producer A", "Producer B", "Producer C"),
  produced = c("errors", "not errors"))
kable(data)

mosaicplot(data, shade=TRUE, off = 5)


errors_producer <- sum(data[, "errors"]) / sum(data)
errors_producer

not_errors_producer <- sum(data[, "not errors"]) /
  sum(data)
not_errors_producer

errors_producer + not_errors_producer


expected <- c(
  (data[, "errors"] + data[, "not errors"]) * errors_producer,
  (data[, "errors"] + data[, "not errors"]) * not_errors_producer)

expected

table_expected <- matrix(expected, ncol=2)
dimnames(table_expected) <- list(
  producer = c("Producer A", "Producer B", "Producer C"),
  produced = c("errors", "not errors"))
kable(table_expected)

x <- sum((data - expected)^2 / expected)
x

plot_chisq2 <- function(sim_data, x, crit_val, title = ""
) {
  annotation_y_val <- max(sim_data$y) * 1/3
  plt <- ggplot(sim_data, aes(x, y)) + ggtitle(title) +
    labs(y = "density") + geom_line() + geom_area(
      data = subset(sim_data, x >= crit_val),
      fill = "red", alpha = 0.24) +
    annotate("segment", x = crit_val, xend = crit_val,
             y = 0, yend = annotation_y_val,
             color = "red") + 
    annotate("text", x = crit_val, y = annotation_y_val,
                                       label = "chi-critical", parse = TRUE) +
    annotate("segment", x = x, xend = x,
             y = 0, yend = annotation_y_val,
             color = "black") + annotate("text",
                                         x = x, y = annotation_y_val* 1.1, label = "chi-observed", parse = TRUE)
  return(plt)}

# alpha = 0.01
alpha = 0.01
df = 2
crit_val1 <- qchisq(alpha, df, lower.tail = FALSE)
crit_val1

sim_data <- data.frame(density(rchisq(125+200+325, df))[c("x", "y")])
plot1 = plot_chisq2(sim_data, x, crit_val1, title="Density plot of critical and observed values alpha=0.01")

p_value <- 1 - pchisq(x, df)
paste(round(p_value,5), "Thus, Reject H0")


# alpha = 0.05
alpha = 0.05
df = 2
crit_val2 <- qchisq(alpha, df, lower.tail = FALSE)
crit_val2

sim_data <- data.frame(density(rchisq(125+200+325, df))[c("x", "y")])
plot2 = plot_chisq2(sim_data, x, crit_val2, title="Density plot of critical and observed values alpha=0.05")

grid.arrange(plot1, plot2, ncol=2)

p_value <- 1 - pchisq(x, df)
paste(round(p_value,5), "Thus, Reject H0")
paste("But")

paste("Chi-obs =", round(x,5), ", Crit_val at 0.01 =", round(crit_val1,5), ", Crit_val at 0.05 =", round(crit_val2,5))
((x > crit_val1) && (x > crit_val2))


#### Exercise 36 ####

data35 <- matrix(c(11, 60, 107, 94), ncol=2)
dimnames(data35) <- list(
  happiness = c("happy", "unhappy"),
  dreamjob = c("Family OK", "Family not OK"))
kable(addmargins(data35))

mosaicplot(data35, col=c(7,5))

numerator <- sum(data35)*(11*94-60*107)^2
numerator
denominator <- sum(data35["happy", ])*sum(data35["unhappy", ])*sum(data35[ ,"Family OK"])*sum(data35[ ,
                                                    "Family not OK"])
denominator
chi_obs <- numerator / denominator
chi_obs

# alpha = 0.05
chi1= qchisq(0.95, 1, ncp=0, lower.tail = TRUE, log.p = FALSE)

# alpha = 0.01
chi2 = qchisq(0.99, 1, ncp=0, lower.tail = TRUE, log.p = FALSE)


paste("Chi-obs =", round(chi_obs,5), ", Crit_val at 0.05 =", round(chi1,5), ", Crit_val at 0.01 =", round(chi2,5))
((chi_obs > chi1) && (chi_obs > chi2))


#### Exercise 37 ####

data35 <- matrix(c(34, 26, 36, 4), ncol=2)
dimnames(data35) <- list(
  happiness = c("suitable", "unsuitable"),
  dreamjob = c("Engineer", "Sales man"))
kable(addmargins(data35))

mosaicplot(data35, col=c(7,5), main="Job application based on role and suitability",
           xlab="Suitability", ylab="Job Role")

numerator <- sum(data35)*(34*4-36*26)^2
numerator
denominator <- sum(data35["suitable", ])*sum(data35["unsuitable", ])*sum(data35[ ,"Engineer"])*sum(data35[ ,"Sales man"])
denominator
chi_obs <- numerator / denominator
chi_obs

# alpha = 0.05
chi1= qchisq(0.95, 1, ncp=0, lower.tail = TRUE, log.p = FALSE)

# alpha = 0.01
chi2 = qchisq(0.99, 1, ncp=0, lower.tail = TRUE, log.p = FALSE)


paste("Chi-obs =", round(chi_obs,5), "/ Crit value at 0.05 =", round(chi1,5), "/ Crit value at 0.01 =", round(chi2,5))
((chi_obs > chi1) && (chi_obs > chi2))


#### Exercise 38 ####
data35 <- matrix(c(48, 20, 22, 10), ncol=2)
dimnames(data35) <- list(
  happiness = c("suitable", "unsuitable"),
  dreamjob = c("Engineer", "Sales man"))
kable(addmargins(data35))

mosaicplot(data35, col=c(7,5), main="Job application based on role and suitability",
           xlab="Suitability", ylab="Job Role")

numerator <- sum(data35)*(48*10-20*22)^2
numerator
denominator <- sum(data35["suitable", ])*sum(data35["unsuitable", ])*sum(data35[ ,"Engineer"])*sum(data35[ ,"Sales man"])
denominator
chi_obs <- numerator / denominator
chi_obs

# alpha = 0.05
chi1= qchisq(0.95, 1, ncp=0, lower.tail = TRUE, log.p = FALSE)
chi1

# alpha = 0.01
chi2 = qchisq(0.99, 1, ncp=0, lower.tail = TRUE, log.p = FALSE)
chi2

paste("Chi-obs =", round(chi_obs,5), "/ Crit value at 0.05 =", round(chi1,5), "/ Crit value at 0.01 =", round(chi2,5))
((chi_obs > chi1) && (chi_obs > chi2))


#### Exercise 39 ####
tips<-read.csv2("tips.csv")

tally(~smoker | time, data = tips)
mosaicplot(smoker ~ time, data = tips, col=c(5,7), main="Association between smoking and time of the day")
xchisq.test(smoker ~ time, data = tips)

paste("p-value is:", 0.4771, "which is greater than 0.05")

#### Exercise 41 ####
ICM<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/ICM.txt",
                stringsAsFactors=F)
inspect(ICM)
tally(~Gender | Highest_level_of_education, data = ICM)
mosaicplot(Gender ~ Highest_level_of_education, data = ICM, col=c(5,7), main="Association between Gender and Education")
xchisq.test(Gender ~ Highest_level_of_education, data=ICM)


paste("p-value is:", 0.1337, "which is greater than 0.05")


#### Exercise 42 ####
ICM<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/ICM.txt",
                stringsAsFactors=F)
inspect(ICM)
tally(~Highest_level_of_education | Do_you_smoke, data = ICM)
mosaicplot(Highest_level_of_education ~ Do_you_smoke, data = ICM, col=c(5,7), main="Association between Education and Smoking")
xchisq.test(Highest_level_of_education ~ Do_you_smoke, data=ICM)


paste("p-value is:", 0.2021, "which is greater than 0.05")


#### Exercise 43 ####
ICM<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/ICM.txt",
                stringsAsFactors=F)
inspect(ICM)
tally(~Transport | Socialmediahours, data = ICM)
mosaicplot(Transport ~ Socialmediahours, data = ICM, col=c(5,7), main="Association between Education and Smoking")
xchisq.test(Transport ~ Socialmediahours, data=ICM)


paste("p-value is:", 0.0052, "which is less than 0.05")


#### Exercise 45 ####
diet<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/diet_paired.txt",
                stringsAsFactors=F)
head(diet, 3)
inspect(diet)
bp <- ggplot(diet, aes(x=before_diet, color=before_diet)) +
  geom_boxplot(color="violet", varwidth = TRUE, fill="slateblue", alpha=0.2) +
  theme(legend.position = "none")+
  background_grid(major = "xy", minor = "none")+
  xlim(85,110)

bp2 = ggplot(diet, aes(x=after_diet, color=before_diet)) +
  geom_boxplot(color="red", varwidth = TRUE, fill="orange", alpha=0.2) + 
  theme(legend.position = "none")+
  background_grid(major = "xy", minor = "none")+
  xlim(85, 110)

grid.arrange(bp, bp2, nrow=2)
wilcox.test(diet$before_diet, diet$after_diet, paired=TRUE)

paste("p-value is:", 0.037, "which is less than 0.05")


#### Exercise 46 ####
students<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/OHS_2020_paired.txt",
                     stringsAsFactors=F)
head(students, 3)
inspect(students)
bp <- ggplot(students, aes(x=OHS_1, color=before_diet)) +
  geom_boxplot(color="violet", varwidth = TRUE, fill="slateblue", alpha=0.2) +
  theme(legend.position = "none")+
  background_grid(major = "xy", minor = "none")+
  xlim(3.5, 5.5)

bp2 = ggplot(students, aes(x=OHS_2, color=before_diet)) +
  geom_boxplot(color="red", varwidth = TRUE, fill="orange", alpha=0.2) + 
  theme(legend.position = "none")+
  background_grid(major = "xy", minor = "none")+
  xlim(3.5, 5.5)

bp3 = ggplot(students, aes(x=OHS_3, color=before_diet)) +
  geom_boxplot(color="green", varwidth = TRUE, fill="brown", alpha=0.2) + 
  theme(legend.position = "none")+
  background_grid(major = "xy", minor = "none")+
  xlim(3.5, 5.5)

grid.arrange(bp, bp2, bp3, nrow=3)


## Remove NA vals
oh1 = students$OHS_1
oh1 = na.omit(oh1)
oh1
length(oh1)

oh2 = students$OHS_2
oh2 = na.omit(oh2)
oh2
length(oh2)

oh3 = students$OHS_3
oh3 = na.omit(oh3)
oh3
length(oh3)


## Normalize the vectors length 

## Create random variables to remove

remove_element = function(vec){
ran1 = sample(min(vec):max(vec),1)
ran1
ran1 = match(c(ran1),vec)
ran1
vec = vec[-ran1]
return(vec)
}

oh1 = remove_element(oh1)
oh2 = remove_element(oh2)
oh2 = remove_element(oh2)
length(oh1)
length(oh2)
length(oh3)


# Test Oh1 & Oh2
wilcox.test(oh1, oh2, paired=TRUE)
paste("p-value is:", 0.058, "which is greater than 0.05")

# Plot Oh1 & Oh2
pd1 <- paired(oh1, oh2)
pl1 = plot(pd1, type = "profile") + theme_bw()

# Test Oh1 & Oh3
wilcox.test(oh1, oh3, paired=TRUE)
paste("p-value is:", 0.018, "which is less than 0.05")

# Plot Oh1 & Oh3
pd2 <- paired(oh1, oh3)
pl2 = plot(pd2, type = "profile") + theme_bw()

# Test Oh2 & Oh3
wilcox.test(oh2, oh3, paired=TRUE)
paste("p-value is:", 0.947, "which is greater than 0.05")

# Plot Oh2 & Oh3
pd3 <- paired(oh2, oh3)
pl3 = plot(pd3, type = "profile") + theme_bw()

grid.arrange(pl1, pl2, pl3, ncol=2)


#### Exercise 49 ####
ICM<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/ICM.txt",
                stringsAsFactors=F)

head(ICM)
inspect(ICM)

wilcox.res <- wilcox.test(Communication_open_direct ~ Siblings, data=ICM)
wilcox.res

paste("p-value is:", 0.0303, "which is less than 0.05")

ggplot(ICM, aes(x=Communication_open_direct, y=Siblings, color=Siblings)) +
  geom_violin(fill="white", alpha=0.4) +
  geom_boxplot()+
  background_grid(major = "xy", minor = "none")+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  scale_fill_brewer(palette="Blues") + theme_classic()


#### Exercise 50 ####
ICM<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/ICM.txt",
                stringsAsFactors=F)

head(ICM)
inspect(ICM)

wilcox.res <- wilcox.test(Mentalhealth ~ Children, data=ICM)
wilcox.res

paste("p-value is:", 0.0912, "which is greater than 0.05")

ggplot(ICM, aes(x=Mentalhealth, y=Children, color=Children)) +
  geom_violin(fill="white", alpha=0.4) +
  geom_boxplot()+
  background_grid(major = "xy", minor = "none")+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  scale_fill_brewer(palette="Blues") + theme_classic()
