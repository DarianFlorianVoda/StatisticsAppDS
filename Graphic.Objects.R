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


### Functions to Generate Objects

#####################

### Helper Functions

# r = radius;
# phi = rotation (counter-clockwise);
pointsCircle = function(n, r, center = c(0,0), phi=0) {
  x = r * cos(seq(0, n-1) * 2*pi/n + phi) + center[1];
  y = r * sin(seq(0, n-1) * 2*pi/n + phi) + center[2];
  return(list(x=x, y=y));
}

#### Tangent circles forming a large circle ####
circlesOnCircle = function(n, r, center = c(0,0), phi=0) {
  R  = r / sin(pi/n);
  xy = pointsCircle(n, r=R, center=center, phi=phi);
  attr(xy, "R") = R;
  return(xy);
}

##### Outside a large circle of given radius #####
circlesOutsideFixedCircle = function(n, r, center = c(0,0), phi=0) {
  r1 = r / (1/sin(pi/n) - 1);
  R1 = r + r1;
  xy = pointsCircle(n, r=R1, center=center, phi=phi);
  attr(xy, "R") = R1;
  attr(xy, "r") = r1;
  return(xy);
}

#### Tangent circles ####

##### Forming large circle of given radius #####
circlesOnFixedCircle = function(n, r, center = c(0,0), phi=0) {
  r1 = r * sin(pi/n);
  xy = pointsCircle(n, r=r, center=center, phi=phi);
  attr(xy, "R") = r1; # reuse same attribute name ???
  return(xy);
}

##### Inside a large circle of given radius #####
circlesInFixedCircle = function(n, r, center = c(0,0), phi=0) {
  r1 = r / (1 + 1/sin(pi/n));
  R  = r - r1;
  xy = pointsCircle(n, r=R, center=center, phi=phi);
  attr(xy, "R") = R;
  attr(xy, "r") = r1;
  return(xy);
}

#### Cell-object resembling a smooth muscle cell ####
cellSmooth = function(x, y, r=1, slope=NULL, lwd=2, N=128, phi=pi) {
  if(is.null(slope)) slope = compute_slope(x, y);
  d = sqrt((x[1] - x[2])^2 + (y[1] - y[2])^2);
  dx = d / N;
  pL = seq(0, d, by=dx);
  pp = shiftPoint(c(x[1], y[1]), d=pL, slope=slope);
  pL = pL + x[1];
  px = pp[,1] - x[1]; px = px * pi / max(abs(px));
  # Margin 1:
  pS = r * sin(px) + pp[,2];
  lst = list(x = pL, y = pS);
  # Margin 2:
  pS = r * sin(px + phi) + pp[,2];
  lst = list(lst, list(x = pL, y = pS));
  #
  lst$lwd = lwd;
  return(lst)
}

  