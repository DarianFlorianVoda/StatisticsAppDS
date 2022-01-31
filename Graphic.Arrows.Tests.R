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
#  -- slope: ascending X, descending X, vertical X, horizontal X,
#     slightly perturbed vertical or horizontal;
# - separate source file with the various tests X;

# Base-Line
# TODO: various origins, e.g.(x0=0, y0=0) vs non-zero;
# Note: x = (x0, x1); y = (y0, y1);
# Testing various origins

### Test: Lines

### Test Different lines on x-axis where x is between 0 and 10
### Point P is the upper limit of the line with x2 = 10; y2 = 5
P = c(10,5)

plot.base()
x = c(0,P[1]); y = c(0, P[2]);
lines(x, y, lwd=2, col="red") 

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


### Test Different lines on x-axis where y is between 0 and 10
plot.base(xlim=c(-2,10), ylim=c(-2,10), axt=c(2))
x = c(0,10); y = c(0, 5);
lines(x, y, lwd=2, col="red")

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

### Test: origin: (0, 0), (0, 1), (0, 2), (0, -2), ... ;
# line
plot.base(xlim=c(-2,10), ylim=c(-2,10), axt=c(1,2))
x = c(0,5); y = c(4, 4);
lines(x, y, lwd=2, col="red")
abline(v=x, col="green", lty=3)

# points(x, y)!
# Test 1
p0 = c(0, 0)
p = reflect(x, y, p0)
points(p0[1], p0[2]); points(p[1], p[2]);
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")

### Test: Reflections

### Horizontal Reflection

# line
plot.base(xlim=c(-2,10), ylim=c(-2,10), axt=c(1,2))
x = c(0,5); y = c(4, 4);
lines(x, y, lwd=2, col="red")
abline(v=x, col="green", lty=3)

# points(x, y)!
# Test 1
p0 = c(2, 6)
p = reflect(x, y, p0)
points(p0[1], p0[2])
points(p[1], p[2]);
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
stopifnot(all(p==c(2,2)))

# Test 2
p0 = c(2.5, 2)
p = reflect(x, y, p0)
points(p0[1], p0[2])
points(p[1], p[2]);
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
# ambiguity: stopifnot(all(p==c(2.6,6)))

# Test 3
p0 = c(1, 1)
p = reflect(x, y, p0)
points(p0[1], p0[2])
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
stopifnot(all(p == c(1,7)))

# Test 4
p0 = c(4, 9)
p = reflect(x, y, p0)
points(p0[1], p0[2])
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
stopifnot(all(p == c(4,-1)))

# Test 5 # out of bound?
p0 = c(6, 6)
p = reflect(x, y, p0)
points(p0[1], p0[2])
points(p[1], p[2]);
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
stopifnot(all(p==c(6,2)))

# Test 6 # out of bound?
p0 = c(-1, -1)
p = reflect(x, y, p0)
points(p0[1], p0[2])
points(p[1], p[2]);
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
stopifnot(all(p==c(-1,9)))

# Test 7 # still a small green line on point?
p0 = c(3, 4)
p = reflect(x, y, p0)
points(p0[1], p0[2])
points(p[1], p[2]);
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
stopifnot(all(p==c(3,4)))

# Test 8
p0 = c(3.5, 4.25)
p = reflect(x, y, p0)
points(p0[1], p0[2])
points(p[1], p[2]);
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
stopifnot(all(p==c(3,4)))

### Vertical Reflection

# line
plot.base(xlim=c(-2,10), ylim=c(-2,10), axt=c(1,2))
x = c(3,3); y = c(1, 8);
lines(x, y, lwd=2, col="red")
abline(v=c(-2,8), col="green", lty=3)

# points(x, y)!
# Test 1
p0 = c(4, 7)
p = reflect(x, y, p0)
points(p0[1], p0[2]) 
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
stopifnot(all(p == c(2,7)))

# Test 2
p0 = c(6, 5)
p = reflect(x, y, p0)
points(p0[1], p0[2]) 
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
stopifnot(all(p == c(0,5)))

# Test 3
p0 = c(8, 3)
p = reflect(x, y, p0)
points(p0[1], p0[2]) 
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
stopifnot(all(p == c(-2,3)))

# Test 4
p0 = c(2, 6)
p = reflect(x, y, p0)
points(p0[1], p0[2]) 
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
stopifnot(all(p == c(4,6)))

# Test 5
p0 = c(3, 4)
p = reflect(x, y, p0)
points(p0[1], p0[2]) 
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
stopifnot(all(p == c(3,4)))

# Test 6 # out of bound?
p0 = c(2, 0)
p = reflect(x, y, p0)
points(p0[1], p0[2]) 
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
stopifnot(all(p == c(4,0)))

### Ascending Reflection

# line
plot.base(xlim=c(-2,10), ylim=c(-2,10), axt=c(1,2))
x = c(0,6); y = c(0, 6);
lines(x, y, lwd=2, col="red")

# points(x, y)!
# Test 1
p0 = c(1, 2)
p = reflect(x, y, p0)
points(p0[1], p0[2]) 
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
stopifnot(all(p == c(2,1)))

# Test 2
p0 = c(2, 4)
p = reflect(x, y, p0)
points(p0[1], p0[2]) 
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
stopifnot(all(p == c(4,2)))

# Test 3
p0 = c(4, 7)
p = reflect(x, y, p0)
points(p0[1], p0[2]) 
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
stopifnot(all(p == c(7,4)))

# Test 4
p0 = c(1, 0)
p = reflect(x, y, p0)
points(p0[1], p0[2]) 
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
stopifnot(all(p == c(0,1)))

# Test 5
p0 = c(4, 4)
p = reflect(x, y, p0)
points(p0[1], p0[2]) 
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
stopifnot(all(p == c(4,4)))


# Test 6 # out of bound?
p0 = c(0, -1)
p = reflect(x, y, p0)
points(p0[1], p0[2]) 
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
stopifnot(all(p == c(-1,0)))

### Descending Reflection

# line
plot.base(xlim=c(-2,10), ylim=c(-2,10), axt=c(1,2))
x = c(0,8); y = c(8, 1);
lines(x, y, lwd=2, col="red")

# points(x, y)!
# Test 1
p0 = c(1, 8)
p = reflect(x, y, p0)
points(p0[1], p0[2]) 
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
# Ambiguity: p has float values stopifnot(all(p == c(0.133,7.009)))
stopifnot(round(p - c(0, 7) - c(15,1)/113, 12) == 0)
# print(p, 13)
# ... (round(p - ...., 12) == 0)

# Test 2
p0 = c(3, 8)
p = reflect(x, y, p0)
points(p0[1], p0[2]) 
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
# Ambiguity: p has float values stopifnot(all(p == c(0.133,7.009)))

# Test 3
p0 = c(6, 7)
p = reflect(x, y, p0)
points(p0[1], p0[2]) 
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
# Ambiguity: p has float values stopifnot(all(p == c(0.133,7.009))) ??

# Test 4
p0 = c(4, 2)
p = reflect(x, y, p0)
points(p0[1], p0[2]) 
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
# Ambiguity: p has float values stopifnot(all(p == c(0.133,7.009))) ??

# Test 5
p0 = c(8, 1)
p = reflect(x, y, p0)
points(p0[1], p0[2]) 
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
stopifnot((all(p == c(8,1))))

### Slightly Perturbed Vertical Reflection

# line
plot.base(xlim=c(-2,10), ylim=c(-2,10), axt=c(1,2))
x = c(3.25,3); y = c(1, 8);
lines(x, y, lwd=2, col="red")
abline(v=c(-2,8), col="green", lty=3)

# points(x, y)!
# Test 1
p0 = c(4, 7)
p = reflect(x, y, p0)
points(p0[1], p0[2]) 
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
# Ambiugity: stopifnot(all(p == c(2.073885,6.931210)))

# Test 2
p0 = c(6, 5)
p = reflect(x, y, p0)
points(p0[1], p0[2]) 
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
# Ambiguity: stopifnot(all(p == c(0,5)))

# Test 3
p0 = c(8, 3)
p = reflect(x, y, p0)
points(p0[1], p0[2]) 
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
# Ambiguity: stopifnot(all(p == c(-2,3)))

# Test 4
p0 = c(3, 8)
p = reflect(x, y, p0)
points(p0[1], p0[2]) 
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
# Ambiguity: stopifnot(all(p == c(3, 8)))

# Test 5
p0 = c(2, 2)
p = reflect(x, y, p0)
points(p0[1], p0[2]) 
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
# Ambiguity: stopifnot(all(p == c(4.43, 2.09)))


### Slightly Perturbed Horizontal Reflection

# line
plot.base(xlim=c(-2,10), ylim=c(-2,10), axt=c(1,2))
x = c(0,5); y = c(4, 4.5);
lines(x, y, lwd=2, col="red")
abline(v=x, col="green", lty=3)

# points(x, y)!
# Test 1
p0 = c(2, 6)
p = reflect(x, y, p0)
points(p0[1], p0[2]); points(p[1], p[2]);
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
# Ambiguity: stopifnot(all(p==c(2,2)))

# Test 2
p0 = c(1, 8)
p = reflect(x, y, p0)
points(p0[1], p0[2])
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
# Ambiguity: stopifnot(all(p == c(1,0)))

# Test 3
p0 = c(3, 5)
p = reflect(x, y, p0)
points(p0[1], p0[2])
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
# Ambiguity: stopifnot(all(p == c(3,3)))

# Test 4
p0 = c(4, 9)
p = reflect(x, y, p0)
points(p0[1], p0[2])
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
# Ambiguity: stopifnot(all(p == c(4,-1)))

# Test 5
p0 = c(4, 0)
p = reflect(x, y, p0)
points(p0[1], p0[2])
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
# Ambiguity: stopifnot(all(p == c(4,-1)))

# Test 6
p0 = c(0, 4)
p = reflect(x, y, p0)
points(p0[1], p0[2])
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
stopifnot(all(p == c(0,4)))

# Test 7 # out of bound
p0 = c(6, 2)
p = reflect(x, y, p0)
points(p0[1], p0[2])
points(p[1], p[2])
lines(c(p0[1], p[1]), c(p0[2], p[2]), col="green")
# Ambiguity: stopifnot(all(p == c(4,-1)))

### Shift point along line:

plot.base()
x = c(0,10); y = c(1, 5);
lines(x, y, lwd=2, col="red")

# Test 1
p0 = c(0, 1)
p = shiftH(p0, x, y, d=1)
points(p0[1], p0[2])
points(p, col="green")

# Test 2
p0 = c(0, 2)
p = shiftH(p0, x, y, d=seq(1, 4, by=0.5))
points(p0[1], p0[2])
points(p, col="blue")

# Test 3
p0 = c(4, 2)
p = shiftH(p0, x, y, d=1)
points(p0[1], p0[2])
points(p, col="green")

# Test 4
p0 = c(0,4)
p = shiftH(p0, x, y, d=seq(1,4, by=0.5))
points(p0[1], p0[2])
points(p, col="blue")

# Test 5
p0 = c(0,4)
p = shiftH(p0, x, y, d=seq(1,4, by=0.5))
points(p0[1], p0[2])
points(p, col="blue")

### Shift Line
l = shift(x, y, d=2)
len = nrow(l) / 2;
sapply(seq(len), function(id) lines(l[c(id, id+len),1], l[c(id, id+len),2], col="orange"))
