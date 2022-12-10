x=10
hist(x, freq=FALSE)
density(x)
hist(x, breaks=seq(0.0,10.5,1.05), freq=F, main="Histogram and empirical density of x")
lines(density(x), add=TRUE)


#define range
p = seq(0,1, length=100000)

data = rbeta(p, 2, 5)
data2 = pbeta(p, 2, 5)

#create plot of Beta distribution with shape parameters 2 and 10
y = plot(data)


breaks=seq(0,1,length.out=13)

h = hist(data, freq = FALSE, col="skyblue", breaks = seq(0,1, length.out=13))
lines(density(data))
text(h$mids,
     h$density,
     labels = h$counts/100,
     adj=c(0.5, -0.5))

prob = pbeta(breaks[-1], 2, 5) - pbeta(breaks[length(breaks)], 2, 5)

text(h$mids,
     h$density,
     labels = prob,
     adj=c(0.5, -2))


### 1.3
dat = rnorm(100000000, mean=7, sd = 2)

mean(dat)
sd(data)

### 1.5

n = 10
x = rexp(n, 1/356)
x

lambda.hat = 1/mean(x)
lambda.hat

1-pexp(2*365, lambda.hat)

plot(seq(0,800,0.1), dexp(seq(0,800,0.1), lambda.hat), type="l")

