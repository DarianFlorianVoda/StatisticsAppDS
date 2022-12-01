library(ggpubr)

data("mtcars")

head(mtcars)

cor(mtcars$mpg, mtcars$wt)

ggscatter(mtcars, x = "wt", y = "mpg",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Weight", ylab = "Miles / gallon"
)

shapiro.test(mtcars$mpg)

shapiro.test(mtcars$wt)


ggqqplot(mtcars$mpg, ylab = "Miles per gallon")

ggqqplot(mtcars$wt, ylab = "Weight")

cor.test(mtcars$mpg, mtcars$wt, method = "pearson")

data("faithful")


head(faithful)

cor(faithful$eruptions, faithful$waiting)


plot(faithful$eruptions, faithful$waiting, xlab="Eruption duration", ylab="Time waited")


ggscatter(faithful, x = "eruptions", y = "waiting",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Eruption duration", ylab = "Time
waited")

shapiro.test(faithful$eruptions)

shapiro.test(faithful$waiting)

ggqqplot(faithful$eruptions, ylab = "Eruption duration")

ggqqplot(faithful$waiting, ylab = "Time waited")

cor.test(faithful$eruptions, faithful$waiting, method = "pearson")



plants <- PlantGrowth
head(plants, 3)
tail(plants, 3)
levels(plants$group)
library(dplyr)
group_by(plants, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
    )

library(ggpubr)
ggboxplot(plants, x = "group", y = "weight",
            color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
            order = c("ctrl", "trt1", "trt2"),
            ylab = "Weight", xlab = "Treatment")


res.aov <- aov(weight ~ group, data = plants)
summary(res.aov)
