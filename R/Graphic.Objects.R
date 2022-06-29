###################
#
# Thesis
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


### Functions to Generate Objects


### Plot Objects formed from circles;
# - convenience function;
#' @export
testFilledCircle = function(xy, r=NULL, R=NULL, lim=NULL, line=TRUE,
                            col="#B0B032", col.line="green", add=FALSE, ...) {
  if(is.null(r)) {
    r = attr(xy, "r");
    if(is.null(r)) stop("Missing r!")
  } else {
    attr(xy, "r") = r;
  }
  if( ! add) {
    x = xy$x; y = xy$y;
    mid = attr(xy, "center");
    if(is.null(lim)) {
      R0  = attr(xy, "R");
      lim = R0 + r + 1;
      lim = c(-lim, lim);
    } else if(length(lim) == 1) {
      lim = c(-lim, lim);
      mid = c(0, 0); # remove center offset;
    } else {
      mid = c(0, 0); # remove center offset;
    }
    plot(x, y, xlim = lim + mid[1], ylim = lim + mid[2]);
  }
  pin = mean(par("pin")) + 0.25;
  par.old = par(pin = c(pin, pin));
  lines.circles(xy, R=R, line=line, fill=col, col.line=col.line, ...)
  par(par.old);
}


#####################

### Helper Functions

# r = radius;
# phi = rotation (counter-clockwise);
#' @export
pointsCircle = function(n, r, center = c(0,0), phi=0) {
  x = r * cos(seq(0, n-1) * 2*pi/n + phi) + center[1];
  y = r * sin(seq(0, n-1) * 2*pi/n + phi) + center[2];
  lst = list(x=x, y=y);
  attr(lst, "R") = r;
  attr(lst, "center") = center;
  return(lst);
}


#### Tangent circles forming a large circle ####
#' @export
circlesOnCircle = function(n, r, center = c(0,0), phi=0) {
  R  = r / sin(pi/n);
  xy = pointsCircle(n, r=R, center=center, phi=phi);
  attr(xy, "R") = R;
  attr(xy, "r") = r;
  return(xy);
}

##### Outside a large circle of given radius #####
#' @export
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
#' @export
circlesOnFixedCircle = function(n, r, center = c(0,0), phi=0) {
  r1 = r * sin(pi/n);
  xy = pointsCircle(n, r=r, center=center, phi=phi);
  attr(xy, "R") = r+r1; # reuse same attribute name ???
  attr(xy, "r") = r1;
  return(xy);
}

##### Inside a large circle of given radius #####
#' @export
circlesInFixedCircle = function(n, r, center = c(0,0), phi=0) {
  r1 = r / (1 + 1/sin(pi/n));
  R  = r - r1;
  xy = pointsCircle(n, r=R, center=center, phi=phi);
  attr(xy, "R") = R;
  attr(xy, "r") = r1;
  return(xy);
}


###################

#### Liposomes ####
#' @export
liposomes = function(n, r, center=c(0, 0), phi=c(0, 0), d=0, ...){
  C1 = circlesOnCircle(n=n[1], r=r, center=center, phi=phi[1])
  C2 = circlesOnCircle(n=n[2], r=r, center=center, phi=phi[2])
  R1 = attr(C1, "R")
  R2 = attr(C2, "R")
  R1 = R1 - r
  R2 = R2 + r
  d2 = (R1-R2-d)/2
  p1 = pointsCircle(n=n[1], r=R1, phi=phi[1], center=center)
  p2 = pointsCircle(n=n[2], r=R2, phi=phi[2], center=center)
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

# n = number of loops;
# N = number of points to draw curve;
# A = amplitude;
# phi = phase shift of sinusoid;
#' @export
helix = function(p1, p2, n=3, A=1, phi=0, N=128, slope=NULL) {
  if(is.null(slope)) {
    x = c(p1[1], p2[1]);
    y = c(p1[2], p2[2]);
    slope = compute_slope(x, y);
    l = sqrt((p1[1] - p2[1])^2 + (p1[2] - p2[2])^2);
  } else {
    if(length(p2) > 1) stop("Provide either: length and slope, or the 2 endpoints!")
    l = p2;
  }
  #
  n = 2*n*pi;
  ninv = 1 / n;
  t  = seq(0, n, length.out=N);
  x  = l*t*ninv;
  y  = A*sin(t + phi);
  #
  if(abs(slope) == Inf) {
    sgn = sign(slope);
    dx = y; dy = x * sgn;
    lst = list(x = p1[1] + dx, y = p1[2] + dy);
    lst = list(lst);
    class(lst) = c("bioshape", class(lst));
    return(lst);
  }
  sdiv = 1 / sqrt(slope^2 + 1);
  if(p1[2] > p2[2] && slope > 0) { x = -x; }
  # Rotation matrix: by column
  # rotm = matrix(sdiv * c(1, s, -s, 1), ncol=2, nrow=2);
  dx = (x - slope*y) * sdiv; # + p1[1];
  dy = (slope*x + y) * sdiv; # + p1[2];
  lst = list(x = p1[1] + dx, y = p1[2] + dy);
  lst = list(lst);
  class(lst) = c("bioshape", class(lst));
  return(lst);
}

#' @export
spirals = function(p1, p2, n=5.5, A=1, phi=0, N=128, slope=NULL) {
  if(is.null(slope)) {
    x = c(p1[1], p2[1]);
    y = c(p1[2], p2[2]);
    slope = compute_slope(x, y);
    l = sqrt((p1[1]- p2[1])^2 + (p1[2]- p2[2])^2);
  } else {
    if(length(p2) > 1) stop("Provide either: length and slope, or the 2 endpoints!")
    l = p2;
  }
  #
  n = 2*n*pi;
  ninv = 1 / n;
  v = l * ninv;
  t = seq(0, n, length.out=N);
  # Rotation matrix: by column
  # rotm = matrix(sdiv * c(1, s, -s, 1), ncol=2, nrow=2);
  if(slope == -Inf || (slope != Inf && p1[2] > p2[1])){
    v = - v;
  } else { phi = phi + pi; }
  x  = v*t;
  y  = A*sin(t + phi);
  xc = x + cos(t + phi);
  #
  if(abs(slope) == Inf) {
    sgn = sign(slope);
    dx  = y;
    dy  = xc;
    lst = list(x = p1[1] + dx, y = p1[2] + dy);
    lst = list(lst);
    class(lst) = c("bioshape", class(lst));
    return(lst);
  }
  sdiv = 1 / sqrt(slope^2 + 1);
  #
  dx = (xc - slope*y) * sdiv; # + p1[1];
  dy = (slope*xc + y) * sdiv; # + p1[2];
  lst = list(x = p1[1] + dx, y = p1[2] + dy);
  lst = list(lst);
  class(lst) = c("bioshape", class(lst));
  return(lst);
}


###########################

#### Cell-like Objects ####

#### Cells resembling a smooth muscle cell ####
#' @export
cellSmooth = function(x, y, r=1, slope=NULL, lwd=1, N=128, phi=pi) {
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


#### brush-Border Cells ####
# p1 = Base of Cell, Point 1;
#' @export
cellBrushBorder = function(p1, w, h, n=6.5, A=1, slope=0, lwd=1, N=128, phi=0) {
  # Cell:
  p11 = p1;
  p12 = shiftPoint(p1,  d=w, slope=slope);
  p21 = shiftLine( p1,  d=h, slope=slope);
  p21 = unlist(p21);
  p22 = shiftPoint(p21, d=w, slope=slope);
  # Brush-Border:
  brush = helix(p21, p22, n=n, A=A, phi=phi, N=N);
  brush[[1]]$x = c(brush[[1]]$x, p22[1], p12[1], p11[1], p21[1], brush[[1]]$x[[1]]);
  brush[[1]]$y = c(brush[[1]]$y, p22[2], p12[2], p11[2], p21[2], brush[[1]]$y[[1]]);
  brush$lwd = lwd;
  return(brush);
}

### Example Enzyme ####
#' @export
enzymeReaction = function(x = c(2,5), y = c(1,1),
                          lbl = c("A", "B", "Enzyme", "Inhibitor"),
                          col = c("black", "black", "black", "red", "red"),
                          dx=1, dy=c(0.1, 0.1, 0.5), dI=c(2, 0.75, 2.4), dH=0.5,
                          lwd=c(1, 2), scale=1) {
  if(length(y) == 1) y = c(y, y);
  slope = compute_slope(x, y);
  l1 = shiftLine(x, y, d = dy[[1]], slope=slope, scale=scale);
  l2 = shiftLine(rev(x), rev(y), d = - dy[[2]], slope=slope, scale=scale);
  #
  arrowSimple(l1$x, l1$y, d = -dH, d.head=c(0, 0.5), col=col[[1]], lwd=lwd[[1]]);
  arrowSimple(l2$x, l2$y, d = dH, d.head=c(0, - 0.5), col=col[[1]], lwd=lwd[[1]]);
  text(x[1] - dx, y[1], lbl[[1]], col=col[[2]]);
  text(x[2] + dx, y[2], lbl[[2]], col=col[[2]]);
  # Enzyme
  midx = sum(x)/2;
  midy = sum(y)/2;
  mid  = shiftLine(midx, midy, d = dy[[3]], slope=slope);
  text(mid$x, mid$y, lbl[[3]], col=col[[3]]);
  # Inhibitor
  if(length(lbl) > 3) {
    slopeT = -1/slope;
    pI = shiftPoint(c(midx, midy), d=dI, slope=slopeT, scale=scale);
    arrowT(pI[1:2, 1], pI[1:2, 2], col=col[[4]], lwd=lwd[[2]]);
    text(pI[3,1], pI[3,2], lbl[[4]], col=col[[5]]);
  }
}
