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

### Library
library(shape)


testFilledCircle = function(xy, r=1, R=NULL, d=NULL, col="#B0B032", line=TRUE, add=FALSE){
  x = xy$x; y = xy$y;
  if(is.null(d)) {
    R0 = attr(xy, "R");
    r = attr(xy, "r");
    d = R0 + r + 1;
  }
  if(!add) {
    plot(x, y, xlim=c(-d, d), ylim=c(-d, d))
  }
  pin = mean(par("pin")) + 0.25;
  par.old = par(pin = c(pin, pin));
  for(id in seq(n)) {
    if(is.null(col)) {
      filledcircle(r1=r, r2=0, mid = c(x[id], y[id]))
    } else {
      filledcircle(r1=r, r2=0, mid = c(x[id], y[id]), col=col);
    }
  }
  if(line) {
    R = if(is.null(R)) attr(xy, "R") else R;
    plotcircle(r=R, lcol="green", col=NULL);
  }
  par(par.old);
}

#### Tests ####

#### Circles distanced ####
n = 10
R = 8
d = R+2;
phi = pi / n;
xy = pointsCircle(n, R, phi=phi);
testFilledCircle(xy, d=R+1);


#### Closed Circles ####
# - n known;
n = 15
r = 1
phi = pi / n; # add some rotation
xy = circlesOnCircle(n, r, phi=phi);
testFilledCircle(xy);

# Radius of Big Circle: known
# - small circles: on the big circle;
R = 7
xy = circlesOnFixedCircle(n, r=R, phi=phi);
testFilledCircle(xy, R=R);


#### Inside Circle ####

### Example 1: Simple
n = 19
R = 15
phi = pi / n; # add some rotation
xy = circlesInFixedCircle(n, R, phi=phi);
testFilledCircle(xy, R=R)


### Example 2:
# Tangent & Inside given Circle
# Outer Circle: known
n = 19
r = 1
phi = pi / n; # add some rotation
xy = circlesOnCircle(n, r, phi=phi);
testFilledCircle(xy)

# Inner Circle: unknown
R = attr(xy, "R") - r;
xy = circlesInFixedCircle(n, r=R, phi=phi);
testFilledCircle(xy, add=TRUE, line=FALSE);


#### Outside Circle ####

# Tangent & Outside given Circle
n = 13
R = 6
phi = pi / n; # add some rotation
xy = circlesOutsideFixedCircle(n, R, phi=phi);
testFilledCircle(xy, R=R);


#####################

#### Cell Shapes ####

### Smooth Muscles / Connective Tissue
plot.base()
lst = cellSmooth(c(1,5), c(2, 7))
lines.object.base(lst, lwd=2)

lst = cellSmooth(c(2,7), c(1, 5), r=0.6)
lines.object.base(lst, lwd=2)