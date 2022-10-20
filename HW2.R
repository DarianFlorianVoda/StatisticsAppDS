# Exercise 13
students<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/students.txt",stringsAsFactors=F)

library(mosaic)
library(ggplot2)
library(viridis)
library(hrbrthemes)
tally(~Grade | Sex, data = students)

ex13 <- data.frame(Sex=rep(c("Men", "Women")),
                     Grade=rep(c(1, 2, 3, 4, 5),2),
                     people=c(4, 4, 10, 3, 7, 10, 8, 17, 5, 14))


ggplot(ex13, aes(x=Grade, y=people, fill=Sex)) + 
  geom_bar(stat="identity", width=0.7, position=position_dodge(width=0.8)) +
  geom_text(
    aes(label = people),
    colour = "white", size = 3,
    vjust = 1.5, position = position_dodge(.8)) +
  ggtitle("Absolute Frequency of Grades depending on Gender") +
  theme(plot.title = element_text(hjust = 0.8),
        legend.position="right", 
        panel.background = element_rect(fill = "linen", 
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white")) + labs(x="Grade", y="People")


# Exercise 15

ex15 = data.frame(salary=c(550, 650, 750, 850, 950, 1050, 1150), workers=c(9, 15, 27, 25, 17, 10, 7))

summary(ex15)

nr_work = sum(ex15$workers)
sum_salaries = sum(ex15$salary*ex15$workers)

mean_val = sum_salaries/nr_work
mean_val

all_salaries = c(rep.int(ex15$salary,ex15$workers))
all_salaries

median_val = median(all_salaries)
median_val

library(ggplot2)
ggplot(ex15, aes(x=salary, y=workers)) + 
  geom_bar(stat="identity", fill="purple") +
  geom_line(aes(x=salary, y=workers),stat="identity",color="red",size=1)+
  geom_vline(aes(xintercept = mean_val), color='blue', lty='dashed', lwd=1) + 
  geom_vline(aes(xintercept = median_val), color='orange', lty='dashed', lwd=1) +
  geom_text(
    aes(label = workers),
    colour = "white", size = 3,
    vjust = 1.5, position = position_dodge(.7)
    ) +
  ggtitle("Median and Mean by Salary") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="right", 
        panel.background = element_rect(fill = "transparent", 
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid")) + 
        labs(x="Salary(Euro)", y="Workers") + 
  scale_x_continuous(breaks = round(seq(min(ex15$salary), max(ex15$salary), by = 100),1)) +
  scale_y_continuous(breaks = round(seq(min(ex15$workers), max(ex15$workers), by = 5),1)) + 
  geom_segment(aes(x = 950, y = 20, xend = mean_val, yend = 22),
               arrow = arrow(length = unit(0.8, "cm")), lwd=1, color="blue") + 
  annotate("text", x=1030, y=20, label= "Mean(826.36)") + 
  geom_segment(aes(x = 950, y = 24, xend = median_val, yend = 25),
             arrow = arrow(length = unit(0.8, "cm")), lwd=1, color="orange") +
  annotate("text", x=1020, y=24, label= "Median(850)")


# Exercise 16
library(ggplot2)
ex16 = data.frame(salary = c(600, 700, 750, 3200), workers=c(1, 1, 1, 1))
mean(ex16$salary)
ex16

par(adj=0.3)
boxplot(ex16$salary ~ ex16$workers, 
        ylab="Employees", xlab="Salary(EUR)", 
        main = "Salaries of Employees",
        col = "orange",
        border = "brown",
        horizontal = TRUE
        )


# Exercise 17
mean_salary = 2000
ex17 = data.frame(salaries = c(2080, 1680), sex=c("M", "F"))
summary(ex17)

# Exercise 18
ex18 = data.frame(body_mass = c(61, 63, 65, 67, 69, 71), athletes=c(5, 18, 42, 27, 8, 2))


nr_athletes = sum(ex18$athletes)
sum_body = sum(ex18$body_mass*ex18$athletes)

mean_val = sum_body/nr_athletes
mean_val

all_body = c(rep.int(ex18$body_mass,ex18$athletes))
all_body

median_val = median(all_body)
median_val

library(ggplot2)
library(hrbrthemes)
ggplot(ex18, aes(x=body_mass, y=athletes)) + 
  geom_area(stat="identity", fill="lightblue") +
  geom_line(aes(x=body_mass, y=athletes),stat="identity",color="#69b3a2",size=2)+
  geom_vline(aes(xintercept = mean_val), color='blue', lty='dashed', lwd=1) + 
  geom_vline(aes(xintercept = median_val), color='orange', lty='dashed', lwd=1) +
  #geom_text(
    #aes(label = athletes), colour = "red", lwd=4, vjust = 1.5, position = position_dodge(.7)) +
  ggtitle("Median and Mean by Body Weight") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="right", 
        panel.background = element_rect(fill = "transparent", 
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid")) + 
  labs(x="Body Weight(Kg)", y="Athletes") + 
  scale_x_continuous(breaks = round(seq(min(ex18$body_mass), max(ex18$body_mass), by = 1),1)) +
  scale_y_continuous(breaks = round(seq(min(ex18$athletes), max(ex18$athletes), by = 3),1)) +
  geom_segment(aes(x = 67, y = 29, xend = mean_val, yend = 26),
               arrow = arrow(length = unit(0.8, "cm")), lwd=1, color="blue") + 
  annotate("text", x=68, y=29, label= "Mean(65.411)") + 
  geom_segment(aes(x = 67, y = 32, xend = median_val, yend = 29),
              arrow = arrow(length = unit(0.8, "cm")), lwd=1, color="orange") +
  annotate("text", x=68, y=32, label= "Median(65)")


# Exercise 20
students<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/students.txt",stringsAsFactors=F)
mean_val = mean(students$Weight_kg)
median_val = median(students$Weight_kg)

d = table(unlist(students$Weight_kg))
d

ex20 = data.frame(weight = c(unique(students$Weight_kg)),  nr_stud=c(d))
ex20$nr_stud
ex20$weight

library(ggplot2)
library(hrbrthemes)
ggplot(ex20, aes(x=weight, y=nr_stud)) + 
  geom_area(stat="identity", fill="lightblue") +
  geom_line(aes(x=weight, y=nr_stud),stat="identity",color="#69b3a2",size=2)+
  geom_vline(aes(xintercept = mean_val), color='blue', lty='dashed', lwd=1) + 
  geom_vline(aes(xintercept = median_val), color='orange', lty='dashed', lwd=1) +
  #geom_text(
  #aes(label = athletes), colour = "red", lwd=4, vjust = 1.5, position = position_dodge(.7)) +
  ggtitle("Median and Mean by Body Weight") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="right", 
        panel.background = element_rect(fill = "transparent", 
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid")) + 
  labs(x="Body Weight(Kg)", y="Students") + 
  scale_x_continuous(breaks = round(seq(min(ex20$weight), max(ex20$weight), by = 5),1)) +
  scale_y_continuous(breaks = round(seq(min(ex20$nr_stud), max(ex20$nr_stud), by = 1),1)) +
  geom_segment(aes(x = 69, y = 5, xend = mean_val, yend = 4),
               arrow = arrow(length = unit(0.8, "cm")), lwd=1, color="blue") + 
  annotate("text", x=72, y=5.5, label= "Mean(65.84)") + 
  geom_segment(aes(x = 68, y = 7, xend = median_val, yend = 6),
               arrow = arrow(length = unit(0.8, "cm")), lwd=1, color="orange") +
  annotate("text", x=72, y=7.5, label= "Median(65)")


#########

library(mosaic)
students<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/students.txt",stringsAsFactors=F)
mean_val1 = mean(students$Weight_kg[students$Smoking=="yes"])
mean_val1

mean_val0 = mean(students$Weight_kg[students$Smoking=="no"])
mean_val0

median_val1 = median(students$Weight_kg[students$Smoking=="yes"])
median_val1

median_val0 = median(students$Weight_kg[students$Smoking=="yes"])
median_val0


df = tally(~Weight_kg | Smoking, data = students)

df
ex20_2 = data.frame(Weight_kg = c(unique(students$Weight_kg)), Smoking = c(df))
ex20_2

bargraph(~Weight_kg | Smoking, data = students, ylab="Students", 
main="Means and medians for body weight depending on smoking behaviour", 
scales=list(x=list(rot=60)), layout=c(1,2))


# Exercise 22
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
students<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/students.txt",stringsAsFactors=F)


d = table(unlist(students$Weight_kg))
d

ex22 = data.frame(weight = c(unique(students$Weight_kg)),  nr_stud=c(d))

ggplot(ex22, aes(x=weight, y=nr_stud)) + 
  geom_violin(trim=FALSE, fill="lightblue")+
  labs(title="Violin plot with Box plot on Students Weight", x="Weight", y = "Students")+
  geom_boxplot(width=0.1)+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(students, aes(x=Weight_kg, y=ID, fill=Smoking)) + 
  geom_violin(trim=FALSE, fill="lightblue")+
  labs(title="Violin plot with Box plot on Students Weight based on Smoking ", x="Weight", y = "Students")+
  geom_boxplot(width=0.1)+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Weight(kg)") + ylab("Students")

# Exercise 24
library(ggplot2)
library(moments)

students<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/students.txt",stringsAsFactors=F)

graph1 <- histogram(students$Weight_kg, ylab="Students", 
                    main="Histogram of Students Body Weight", 
                    xlab="Weight(kg)", 
                    type="count"
)

graph2 <- densityplot(~Weight_kg, data = students, main="Density of Students Body Weight") 
plot(graph1, position=c(0, 0, 1, 1))
plot(graph2, position=c(0, 0, 1, 1))

skewness(students$Weight_kg)
kurtosis(students$Weight_kg)

# Exercise 26

library(ggplot2)
library(mosaic)
students<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/students.txt",stringsAsFactors=F)



ggplot(students, aes(Weight_kg)) +
geom_histogram(bins=sqrt(length(students$Weight_kg)), fill="blue") +
xlab("Weight(kg)") + ylab("Students")

ggplot(students, aes(x=Weight_kg)) + 
      geom_density(fill="lightblue") + 
      geom_vline(aes(xintercept=mean(Weight_kg)),
              color="blue", linetype="dashed", size=1) +
  xlab("Weight(kg)") + ylab("Density") +
  geom_segment(aes(x = 70, y = 0.03, xend = mean(students$Weight_kg), yend = 0.02),
               arrow = arrow(length = unit(0.8, "cm")), lwd=1, color="orange") +
  annotate("text", x=71, y=0.032, label= "Mean(65.84)")
