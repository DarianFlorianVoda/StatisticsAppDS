###################
#
# Thesis
#
# Title: Diagram Generator
#
# Candidate: Darian Voda
# Faculty of Mathematics and Informatics, UVT
#
# Coordinator:
#   Prof. Daniela Zaharie
#   Dr. med. Leonard Mada (Syonic SRL)
#
# in collaboration with Syonic SRL
#
# GitHub: https://github.com/DarianFlorianVoda/Diagram-Generator


#### Tests ####
library(shape)
#### Circles distanced ####
n = 10
r = 8
phi = pi / n;
xy = pointsCircle(n, r, phi=phi);
x = xy$x; y = xy$y;
d = r + 2; # plot limits
plot(x, y, xlim=c(-d, d), ylim=c(-d, d))
for(id in seq(n)) {
  filledcircle(r2=0, mid = c(x[id], y[id]))
}

#### Close Circles ####
n = 15
r = 1
phi = pi / n; # add some rotation
xy = circlesOnCircle(n, r, phi=phi);
x = xy$x; y = xy$y;
R = attr(xy, "R");
d = R + r + 1;
plot(x, y, xlim=c(-d, d), ylim=c(-d, d))
par.old = par(pin = c(4.8, 4.8))
for(id in seq(n)) {
  filledcircle(r1=r, r2=0, mid = c(x[id], y[id]))
}
par(par.old)

# Radius of Big Circle: known
R = R-2
xy = circlesOnFixedCircle(n, r=R, phi=phi);
x = xy$x; y = xy$y;
r = attr(xy, "R");
par.old = par(pin = c(4.8, 4.8))
plot.base()
for(id in seq(n)) {
  filledcircle(r1=r, r2=0, mid = c(x[id], y[id]))
}
par(par.old)

#### Inside Circle ####

### Outer Circle
n = 19
r = 1
phi = pi / n; # add some rotation
xy = circlesOnCircle(n, r, phi=phi);
x = xy$x; y = xy$y;
R = attr(xy, "R");
d = R + r + 1;
plot(x, y, xlim=c(-d, d), ylim=c(-d, d))
par.old = par(pin = c(4.8, 4.8))
for(id in seq(n)) {
  filledcircle(r1=r, r2=0, mid = c(x[id], y[id]))
}
par(par.old)

# Inner Circle: unknown
R = R - r;
xy = circlesInFixedCircle(n, r=R, phi=phi);
x = xy$x; y = xy$y;
r = attr(xy, "r");
par.old = par(pin = c(4.8, 4.8))
for(id in seq(n)) {
  filledcircle(r1=r, r2=0, mid = c(x[id], y[id]))
}
par(par.old)

#### Outside Circle ####

###
n = 13
R = 6
phi = pi / n; # add some rotation
xy = circlesOutsideFixedCircle(n, R, phi=phi);
x = xy$x; y = xy$y;
r = attr(xy, "r");
d = R + 2*r + 1;
plot(x, y, xlim=c(-d, d), ylim=c(-d, d))
par.old = par(pin = c(4.8, 4.8))
for(id in seq(n)) {
  filledcircle(r1=r, r2=0, mid = c(x[id], y[id]))
}
par(par.old)


#### Smooth Muscles / Connective Tissue ####
plot.base()
lst = cellSmooth(c(1,5), c(2, 7))
lines.object.base(lst, lwd=2)

lst = cellSmooth(c(2,7), c(1, 5), r=0.6)
lines.object.base(lst, lwd=2)
