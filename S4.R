library(mosaic)
library(ggplot2)
library(latex2exp)
library(gridExtra)
library(knitr)

data <- matrix(c(227, 193, 261, 199, 651 - 227, 740
                 - 193, 735 - 261, 557 - 199), ncol=2)
dimnames(data) <- list(
  producer = c("Line 1", "Line 2", "Line 3", "Line 4"),
  produced = c("errors", "not errors"))
kable(data)

mosaicplot(data)


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
table_expected
dimnames(table_expected) <- list(
  producer = c("Line 1", "Line 2", "Line 3", "Line 4"),
  produced = c("errors", "not errors"))
kable(table_expected)

x <- sum((data - expected)^2 / expected)
x


alpha <- 0.05
df <- 3
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

sim_data <- data.frame(density(rchisq(500, df))[c("x", "y")])
plot_chisq(sim_data, x, crit_val)

p_value <- 1 - pchisq(x, df)
p_value


### Ex 35
data35 <- matrix(c(217, 101, 114, 14), ncol=2)
dimnames(data35) <- list(
  happiness = c("happy", "unhappy"),
  dreamjob = c("Dream job", "Any job"))
kable(addmargins(data35))
mosaicplot(data35)


numerator <- sum(data35)*(217*14-101*114)^2
numerator
denominator <- sum(data35["happy", ])*sum(data35["unhappy", ])*sum(data35[ ,"Dream job"])*sum(data35[ ,
                                                    "Any job"])
denominator
chi_obs <- numerator / denominator
chi_obs

qchisq(0.95, 1, ncp=0, lower.tail = TRUE, log.p =FALSE)
qchisq(0.99, 1, ncp=0, lower.tail = TRUE, log.p =FALSE)


# Association 
library(mosaic)
tally(~smoker | day, data = tips)


ICM<-read.delim("C:/Users/daria/OneDrive/Desktop/Master - AppDS/Statistics/Datasets-20221007/ICM.txt",
                stringsAsFactors=F)

inspect(ICM)

tally(~Gender | Timewithfriends, data = ICM)
