students<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/students.txt",stringsAsFactors=F)


### Problem number 5
tab <- matrix(c(40, 30, 50, 60, 20), ncol=1, byrow= TRUE)
colnames(tab) <- c('Frequency')
rownames(tab) <- c('Overtaking','Right of way','
                      Alcohol','Speeding','Other')
tab <- as.table(tab)


### V1
Reason <-c('Overtaking','Right of way','Alcohol','
            Speeding','Other')
Frequency <-c(40, 30, 50, 60, 20)
exercise6 <-data.frame(col1 = Reason, col2 =
                           Frequency)

### V2

ex6 <- data.frame(Frequency = c(40, 30, 50, 60, 20),
                  Reason = c('Overtaking','Right of way','Alcohol'
                              ,'Speeding','Other'))

### barchart
barplot(Frequency ~ Reason, ex6, col = 1:5)
legend("top", ex6$Reason, fill = 1:5)


### ggplot2

library(ggplot2)
ggplot(ex6, aes(x = Reason, y = Frequency, fill =
                    Reason)) +
geom_col()




