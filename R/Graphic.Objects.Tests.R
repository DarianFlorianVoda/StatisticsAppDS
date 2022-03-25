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


### External Libraries: shape


testFilledCircle = function(xy, r, R=NULL, lim=NULL, col="#B0B032", line=TRUE, add=FALSE, ...) {
  x = xy$x; y = xy$y;
  if(missing(r)) {
    r = attr(xy, "r");
    if(is.null(r)) stop("Missing r!")
  }
  if(is.null(lim)) {
    R0  = attr(xy, "R");
    lim = R0 + r + 1;
  }
  if(!add) {
    plot(x, y, xlim=c(-lim, lim), ylim=c(-lim, lim))
  }
  pin = mean(par("pin")) + 0.25;
  par.old = par(pin = c(pin, pin));
  n = length(x)
  for(id in seq(n)) {
    if(is.null(col)) {
      shape::filledcircle(r1=r, r2=0, mid = c(x[id], y[id]), ...)
    } else {
      shape::filledcircle(r1=r, r2=0, mid = c(x[id], y[id]), col=col, ...);
    }
  }
  if(line) {
    R = if(is.null(R)) attr(xy, "R") else R;
    shape::plotcircle(r=R, lcol="green", col=NULL);
  }
  par(par.old);
}


#### Tests ####

#### Circles distanced ####
n = 10
R = 8
r = 1
phi = pi / n;
xy = pointsCircle(n, R, phi=phi);
testFilledCircle(xy, r=r, lim = R+1, lcol=1);


#### Closed Circles ####
# - n known, R unknown;
n = 15
r = 1
phi = pi / n; # add some rotation
xy = circlesOnCircle(n, r, phi=phi);
testFilledCircle(xy);

### Radius of Big Circle: known
# - small circles: on the big circle;
n = 15
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


#### Liposome ####
plot.base(xlim=c(-10,10), ylim=c(-10,10))
lst = liposomes(c(30, 15), r=0.5)
testFilledCircle(lst[[1]], line = FALSE, add = TRUE)
testFilledCircle(lst[[2]], line = FALSE, add = TRUE)
lines.object.base(lst[3], lwd=1)

