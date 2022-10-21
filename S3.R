library(mosaic)
xpnorm(1, mean=0, sd=5)


low.prob=xpnorm(170, mean=180, sd=10)
high.prob=xpnorm(190, mean=180, sd=10)

diff.prob=high.prob-low.prob

diff.prob


# Ex. 28
mu=180
sigma=10
lower.bound= (1-0.95)/2
upper.bound = (1-0.95)/2+0.95

interval = c(qnorm(lower.bound, mu, sigma),
             qnorm(upper.bound, mu, sigma))
interval

data <- data.frame(density(rnorm(500, mu, sigma))[c(
  "x", "y")])
ggplot(data, aes(x, y)) +
geom_line() +
geom_area(
data=subset(data, x > interval[1] & x < interval[2]),
fill = "blue", alpha=0.24) +
annotate("text", x=mu, y=mean(data$y), label="95%")


# Ex 31
xpnorm(1000, mean=1020, sd=10)
xqnorm(0.005, mean=1020, sd=10)
xqnorm(0.995, mean=1020, sd=10)


mu <- 1020
sigma <- 10
interval.99 <- c(
  qnorm(.01/2, mu, sigma), qnorm(1 - .01/2, mu, sigma)
  )
interval.99

data <- data.frame(density(rnorm(500, mu, sigma))[c(
  "x", "y")])
ggplot(data, aes(x, y)) +
  geom_line() +
  geom_area(
    data=subset(data, x > interval.99[1] & x < interval.99[2]),
  fill="blue", alpha=0.24) +
  annotate(
    "text",
    x=mean(c(min(data$x), max(data$x))),
    y=mean(data$y),
label="99%")


# Data Analysis
library(mosaic)
download.file("https://goo.gl/whKjnl", destfile="tips.csv")
tips<-read.csv2("tips.csv")
inspect(tips)
bargraph(~sex, data = tips)
tally(~sex, format="proportion", data = tips)
tally(~sex, data = tips)
bargraph(~sex | time, data = tips)

tally(~sex | time, data = tips)
tally(~sex | time, format="proportion", data = tips)

tally(~smoker | day, format="proportion", data =
        tips)

tally(~day | smoker, format="proportion", data =
        tips)
c = tally(~day | smoker, format="proportion", data =
            tips)
