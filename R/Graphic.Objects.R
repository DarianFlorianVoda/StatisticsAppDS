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
  lst = list(x=x, y=y);
  attr(lst, "R") = r;
  return(lst);
}


#### Tangent circles forming a large circle ####
circlesOnCircle = function(n, r, center = c(0,0), phi=0) {
  R  = r / sin(pi/n);
  xy = pointsCircle(n, r=R, center=center, phi=phi);
  attr(xy, "R") = R;
  attr(xy, "r") = r;
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
  attr(xy, "R") = r+r1; # reuse same attribute name ???
  attr(xy, "r") = r1;
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

#### Liposomes ####
liposomes = function(n, r, center=c(0, 0), phi=c(0, 0), d=0, ...){
  C1 = circlesOnCircle(n=n[1], r=r, center=center, phi=phi[1])
  C2 = circlesOnCircle(n=n[2], r=r, center=center, phi=phi[2])
  R1 = attr(C1, "R")
  R2 = attr(C2, "R")
  R1 = R1 - r
  R2 = R2 + r
  d2 = (R1-R2-d)/2
  p1 = pointsCircle(n=n[1], r=R1, phi=phi[1])
  p2 = pointsCircle(n=n[2], r=R2, phi=phi[2])
  fn = function(id, p1, p2, d){
    p1 = c(p1$x[id], p1$y[id])
    slope = compute_slope(x=c(p1[1], p2[1]), y=c(p1[2], p2[2]))
    if(p1[1] > center[1]){
      d = -d;
    }
    pp = shiftPoint(p1, slope=slope, d=d)
    data.frame(x=c(p1[1], pp[1]), y=c(p1[2], pp[2]), id=id)
  }
  l1 = lapply(seq(n[1]), fn, p1, center, d2)
  l2 = lapply(seq(n[2]), fn, p2, center, -d2)

  l1 = do.call(rbind, l1)
  l2 = do.call(rbind, l2)

  l2$id = l2$id + nrow(l1)
  l = rbind(l1, l2)

  return(list(C1=C1, C2=C2, l=l))

}

