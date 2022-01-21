###################
#
# Thesis
#
# Title: ...
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
# GitHub/Gitlab/...: TODO


### Tests & Examples

#####################
#TODO: source correct the directory path

# Set your own path to your working directory
path = "C:\\Users\\daria\\OneDrive\\Desktop\\Licenta"
setwd(path)
source(paste(path, "Graphic.Arrows.R", sep="\\"));


### Test Helper Functions

### Example 1:

# Base-Line
# TODO: various origins, e.g.(x0=0, y0=0) vs non-zero;
# Note: x = (x0, x1); y = (y0, y1);

plot.base()
x = c(0,10); y = c(1, 5);
lines(x, y, lwd=2, col="red")

### Shift point along line:
p = shiftH(c(0,1), x, y, d=1)
points(p, col="green")
# only as example:
p = shiftH(c(0,2), x, y, d=seq(1, 4, by=0.5))
points(p, col="blue")


### Reflected point:
# points(x, y)!
p0 = c(1, 2)
p = reflect(x, y, p0)
points(p0[1], p0[2]); points(p[1], p[2]);
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")

#
p0 = c(4, 2)
p = reflect(x, y, p0)
points(p0[1], p0[2]); points(p[1], p[2]);
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")

#
p0 = c(5, 1)
p = reflect(x, y, p0)
points(p0[1], p0[2]); points(p[1], p[2]);
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")

### Shift Line
l = shift(x, y, d=2)
len = nrow(l) / 2;
sapply(seq(len), function(id) lines(l[c(id, id+len),1], l[c(id, id+len),2], col="orange"))

#################
#################


### Arrows:

### Ex 1:
plot.base()
x = c(0,10); y = c(1, 5);
arrowDH(x, y, lwd=2, asD=TRUE);

### Ex 2:
x = c(0, 6); y = c(1, 6);
arrowInvH(x, y, lwd=2);

### Ex 3:
arrowDH(x, y*2 - 2, d = 0.3, lwd=2, col="blue", asD=TRUE);


#############
### Other ###

### Test: Banded lines
plot.base()
lineBanded(c(0,5), c(1, 8), lwd=2.5)
lineBanded(c(1,7), c(0, 5), lwd=2.5)
lineBanded(c(0,6), c(6, 0), lwd=2.5, col="green")


####################
####################

### Derivation:

### Reflection:
# - slope.orthogonal = - 1 / slope;
# - intersection between base-line and reflexion line:
# y.int = (x.int - x[1])*slope + y[1] # Initial Line
# y.int = - (x.int - p[1])/slope + p[2]; # p[1] = x; p[2] = y;
# =>
# x.int = (x[1]*slope + p[1]/slope - y[1] + p[2]) / (slope + 1/slope);
# y.int = - (x.int - p[1])/slope + p[2];


### Shift:
x = c(0,10); y = c(1, 5);
slope = compute_slope(x, y);
# y.sh = - (x.sh - x[1]) / slope + y[1];
# (x.sh - x[1])^2 + (y.sh - y[1])^2 = d^2
# =>
#sl.o = - 1 / slope;
#(sl.o^2 + 1)*x.sh^2 - 2*x1*(sl.o^2 + 1)*x.sh + x1^2*(sl.o^2 + 1) - d^2 # = 0

# TODO:
# - verification of formulas; - quite completed
# - test cases for various types of base-lines and points;
# - base line:
#  -- origin: (0, 0), (0, 1), (0, 2), (0, -2), ...;
#  -- slope: ascending, descending, vertical, horizontal,
#     slightly perturbed vertical or horizontal;
# - separate source file with the various tests;

# Base-Line
# TODO: various origins, e.g.(x0=0, y0=0) vs non-zero;
# Note: x = (x0, x1); y = (y0, y1);
# Testing various origins

P = c(10,5)

#x0 = [0, 10]; y0 = 0
plot.base()
x = c(0,P[1]); y = c(0, P[2]);
lines(x, y, lwd=2, col="red") # Why is it right?

x = c(1,P[1]); y = c(0, P[2]);
lines(x, y, lwd=2, col="red")

x = c(2,P[1]); y = c(0, P[2]);
lines(x, y, lwd=2, col="red")

x = c(3,P[1]); y = c(0, P[2]);
lines(x, y, lwd=2, col="red")

x = c(4,P[1]); y = c(0, P[2]);
lines(x, y, lwd=2, col="red")

x = c(5,P[1]); y = c(0, P[2]);
lines(x, y, lwd=2, col="red")

x = c(6,P[1]); y = c(0, P[2]);
lines(x, y, lwd=2, col="red")

x = c(7,P[1]); y = c(0, P[2]);
lines(x, y, lwd=2, col="red")

x = c(8,P[1]); y = c(0, P[2]);
lines(x, y, lwd=2, col="red")

x = c(9,P[1]); y = c(0, P[2]);
lines(x, y, lwd=2, col="red")

x = c(10,P[1]); y = c(0, P[2]);
lines(x, y, lwd=2, col="red")


#x0 = 0; y0 = [0,10]
plot.base(xlim=c(-2,10), ylim=c(-2,10), axt=c(2))
x = c(0,10); y = c(0, 5);
lines(x, y, lwd=2, col="red") # Why is it right?

x = c(0,P[1]); y = c(1, P[2]);
lines(x, y, lwd=2, col="red")

x = c(0,P[1]); y = c(2, P[2]);
lines(x, y, lwd=2, col="red")

x = c(0,P[1]); y = c(3, P[2]);
lines(x, y, lwd=2, col="red")

x = c(0,P[1]); y = c(4, P[2]);
lines(x, y, lwd=2, col="red")

x = c(0,P[1]); y = c(5, P[2]);
lines(x, y, lwd=2, col="red")

x = c(0,P[1]); y = c(6, P[2]);
lines(x, y, lwd=2, col="red")

x = c(0,P[1]); y = c(7, P[2]);
lines(x, y, lwd=2, col="red")

x = c(0,P[1]); y = c(8, P[2]);
lines(x, y, lwd=2, col="red")

x = c(0,P[1]); y = c(9, P[2]);
lines(x, y, lwd=2, col="red")

x = c(0,P[1]); y = c(10, P[2]);
lines(x, y, lwd=2, col="red")

### Horizontal
plot.base(xlim=c(-2,10), ylim=c(-2,10), axt=c(1,2))
x = c(0,5); y = c(4, 4);
lines(x, y, lwd=2, col="red") # Why is it right?
abline(v=x, col="green", lty=3)

p0 = c(2, 6)
p = reflect(x, y, p0)
points(p0[1], p0[2]); points(p[1], p[2]);
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
# -- origin: (0, 0), (0, 1), (0, 2), (0, -2), ...;

plot.base()
P = c(0, 0)
x = c(0,P[1]); y = c(0, P[2]);
lines(x, y, lwd=2, col="red")

### Shift point along line:
p = shiftH(c(0,1), x, y, d=1)
points(p, col="green")
# only as example:
p = shiftH(c(0,2), x, y, d=seq(1,4, by=0.5))
points(p, col="blue")


### Reflected point:
# points(x, y)!
p0 = c(1, 2)
p = reflect(x, y, p0)
points(p0[1], p0[2]); points(p[1], p[2]);
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")

#
p0 = c(4, 2)
p = reflect(x, y, p0)
points(p0[1], p0[2]); points(p[1], p[2]);
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")

#
p0 = c(5, 1)
p = reflect(x, y, p0)
points(p0[1], p0[2]); points(p[1], p[2]);
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")

### Shift Line
l = shift(x, y, d=2)
len = nrow(l) / 2;
sapply(seq(len), function(id) lines(l[c(id, id+len),1], l[c(id, id+len),2], col="orange"))



