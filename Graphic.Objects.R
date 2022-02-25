n = 10
r = 8
x = r * cos(seq(0, n-1) * 2*pi/n)
y = r * sin(seq(0, n-1) * 2*pi/n)
plot(x, y)
library(shape)
filledcircle(r2=0, mid = c(x[1], y[1]))
for(id in seq(n)) {
  filledcircle(r2=0, mid = c(x[id], y[id]))
}

r = r-2
x = r * cos(seq(0, n-1) * 2*pi/n)
y = r * sin(seq(0, n-1) * 2*pi/n)
for(id in seq(n)) {
  filledcircle(r2=0, mid = c(x[id], y[id]))
}
