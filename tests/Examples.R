###################
#
# Bachelor Thesis
#
# Title: BioShapes
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

#### Helper Functions ####

### Example 1:

# Base-Line
# Note: x = (x0, x1); y = (y0, y1);

plot.base()
x = c(0,10); y = c(1, 5);
lines(x, y, lwd=2, col="red")

### Shift point along line:
p = shiftPoint(c(0,1), x, y, d=1)
points(p, col="green")
# only as example:
p = shiftPoint(c(0,2), x, y, d=seq(1, 4, by=0.5))
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
l = shiftLine(x, y, d=2)
len = nrow(l) / 2;
sapply(seq(len), function(id) lines(l[c(id, id+len),1], l[c(id, id+len),2], col="orange"))

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
# x = c(0,10); y = c(1, 5);
# slope = compute_slope(x, y);
# y.sh = - (x.sh - x[1]) / slope + y[1];
# (x.sh - x[1])^2 + (y.sh - y[1])^2 = d^2
# =>
#sl.o = - 1 / slope;
#(sl.o^2 + 1)*x.sh^2 - 2*x1*(sl.o^2 + 1)*x.sh + x1^2*(sl.o^2 + 1) - d^2 # = 0

#### Arrows ####

all_arrows = function(){
  # Plot
  plot.base()
  text(4, 10,
       "Arrows with different ArrowHeads")
  # Simple ArrowHead
  x = c(-1, -1); y = c(7, 9);
  d=-0.5;
  d.head=c(-0.5,0.5)
  a1 = arrowSimple(x, y, d=d, d.head=d.head, lwd=2);
  text(-1, 6.7,
       "Simple", cex = 0.75);

  # Double ArrowHead
  x = c(1, 1); y = c(7, 9);
  d = -0.3;
  d.head=-0.5
  a2 = arrowDouble(x, y, d=d, d.head=d.head, lwd=2);
  text(1, 6.7,
       "Double", cex = 0.75);

  # Inverted ArrowHead
  x = c(3, 3); y = c(7, 9);
  d=0.5;
  d.head=c(-0.5,0.5)
  a3 = arrowInverted(x, y, d=d, d.head=d.head, lwd=2);
  text(3, 6.7,
       "Inverted", cex = 0.75);

  # Diamond ArrowHead
  x = c(5, 5); y = c(7, 9);
  d.head=0.5;
  d=-0.5;
  arrowDiamond(x, y, d=d, d.head=d.head, lwd=2, join=0);
  text(5, 6.7,
       "Diamond", cex = 0.75);

  # T Shape ArrowHead
  x = c(7, 7); y = c(7, 9);
  arrowT(x, y, d=-1, lwd=2);
  text(7, 6.7,
       "T Shape", cex = 0.75);

  # Measurement ArrowHead
  x = c(9.5, 9.5); y = c(7, 9);
  arrowMeasure(x, y, d=-1, lwd=2);
  text(9.5, 6.7,
       "Measurement", cex = 0.70);
  
  abline(h=9, col="green")

  # X Shape ArrowHead
  x = c(-1, -1); y = c(3, 5);
  arrowX(x, y, d=0.5, lwd=2);
  text(-1, 2.7,
       "X Shape", cex = 0.70);

  # Square Shape ArrowHead
  x = c(1, 1); y = c(3, 5);
  arrowSquare(x, y, d=0.5, lwd=2);
  text(1, 2.7,
       "Square Shape", cex = 0.70);

  # Multiple-Lined ArrowHead
  n = 3; d = 0.5;
  x = c(3.5, 3.5); y = c(3, 5);
  arrowN(x, y, n=n, d=d, lwd=2);
  text(3.5, 2.7,
       "Multiple-Lined", cex = 0.70);

  # Double Lined Inverted ArrowHead
  x = c(6.5, 6.5); y = c(3, 5);
  arrowDoubleInverted(x, y, d=-0.3, lwd=2);
  text(6.5, 2.7,
       "Double-Lined Inverted", cex = 0.70);

  # Solid Circle ArrowHead #####
  x = c(9.5, 9.5); y = c(3, 5);
  arrowCircle(x, y, r=0.5, lwd=2);
  text(9.5, 2.7,
       "Solid Circle", cex = 0.70);
  
  abline(h=5, col="green")

  # Solid Square ArrowHead
  x = c(-1, -1); y = c(-1, 1);
  arrowSolidSquare(x, y, d=-0.5, lwd=2);
  text(-1, -1.3,
       "Solid Square", cex = 0.70);

  # Triangle ArrowHead
  x = c(1, 1); y = c(-1, 1);
  d = -0.5;
  a1 = arrowTriangle(x, y, d=d, lwd=2);
  text(1, -1.3,
       "Triangle", cex = 0.70);
  
  abline(h=1, col="green")
}
all_arrows()



### Ex 1:
plot.base()
x = c(0,10); y = c(1, 5);
arrowDH(x, y, lwd=2);

### Ex 2:
x = c(0, 6); y = c(1, 6);
arrowInvH(x, y, lwd=2);

### Ex 3:
arrowDH(x, y*2 - 2, d = 0.3, lwd=2, col="blue");

#### All Arrows ####

all_arrows()


#### Banded lines ####
plot.base()
lineBanded(c(0,5), c(1, 8), lwd=2.5)
lineBanded(c(1,7), c(0, 5), lwd=2.5)
lineBanded(c(0,6), c(6, 0), lwd=2.5, col="green")
