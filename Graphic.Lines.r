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
# Github/Gitlab/...: TODO


### Initial Ideas


####################

### Helper Functions

# - reflect;
# - shift;

#Computation of slope
compute_slope = function(x, y) {
  if(length(x) < 2 || length(y) < 2) {
    stop("The base-line requires 2 points!");
    # TODO: handle if more than 2 points!
  }
  
  slope = (y[[2]] - y[[1]]) / (x[[2]] - x[[1]]);
  return(slope);
}

### Reflect Point across line
# p = c(x, y) # the point to reflect;
reflect = function(x, y, p, slope=NULL) {
  if(is.null(slope)) {
    if(length(x) < 2 || length(y) < 2) {
      stop("The base-line requires 2 points!");
      # TODO: handle if more than 2 points!
      }
    slope = compute_slope(x, y) #(y[[2]] - y[[1]]) / (x[[2]] - x[[1]]);
  }
  
  if(slope == 0) {
    # Horizontal Line
  } else if(x[[1]] == x[[2]]) {
    # Vertical Line
  }
  
  sl.orto = - 1 / slope;
  # intersection point:
  div = slope - sl.orto; # div always > 0!
  x.int = (x[1]*slope - p[1]*sl.orto - y[1] + p[2]) / div;
  y.int = (x.int - p[1])*sl.orto + p[2];
  print(c(x.int, y.int))
  # Reflected point:
  x.rfl = 2*x.int - p[1];
  y.rfl = 2*y.int - p[2];
  return(c(x.rfl, y.rfl));
}

### Shift line
# d = distance to shift (translate);
shift = function(x, y, d=2, slope=NULL) {
  if(is.null(slope)) {
    if(length(x) < 2 || length(y) < 2) {
      stop("The base-line requires 2 points!");
      # TODO: handle if more than 2 points!
      }
    slope = compute_slope(x, y) #(y[[2]] - y[[1]]) / (x[[2]] - x[[1]]);
  } else {
    if(missing(y)) {
      # both coordinates encoded using parameter x;
      y = x[2]; x = x[1];
    }
  }
  
  sl.orto = - 1 / slope;
  sl2 = (sl.orto^2 + 1);
  delta = d / sl2;
  # shift Start- & End-points:
  shift.f = function(x, y) {
    x.sh = c(x + delta, x - delta);
    y.sh = (x.sh - x)*sl.orto + y;
    cbind(x.sh, y.sh);
  }
  
  rez = lapply(seq(length(x)), function(id) shift.f(x[id], y[id]))
  rez = do.call(rbind, rez);
  colnames(rez) = c("x", "y");
  return(rez);
}

# shift point p along line defined by (x, y);
# d = distance;
shiftH = function(p, x, y, d=1, slope=NULL) {
  if(is.null(slope)) {
    if(length(x) < 2 || length(y) < 2) {
      stop("The base-line requires 2 points!");
    # TODO: handle if more than 2 points!
    }
    slope = compute_slope(x, y) #(y[[2]] - y[[1]]) / (x[[2]] - x[[1]]);
  }
  if(length(p) < 2) {
    stop("Point needs both x & y coordinates!");
  }
  
  slope.sqrt = 1 / sqrt(slope^2 + 1);
  dx = d * slope.sqrt;
  dy = dx * slope;
  xsh = p[[1]] + dx;
  ysh = p[[2]] + dy;
  return(cbind(x=xsh, y=ysh));
}

### Generate complex lines

### Complex Lines:
lineBanded = function(x, y, w=0.1, delta=0.25, lwd=1.5, lty=1, n=NULL, col="black", slope=NULL) {
  if(is.null(slope)) {
    slope = compute_slope(x, y);
  }
  lsh = shift(x, y, d=w, slope=slope);
  distxy = sqrt((x[1] - x[2])^2 + (y[1] - y[2])^2);
  if(is.null(n)) {
    # TODO: use also lwd for better accuracy;
    n = distxy / delta;
  }
  # partition lines:
  dx = lsh[3,1] - lsh[1,1]; dx = dx / n;
  dy = lsh[3,2] - lsh[1,2]; dy = dy / n;
  xup = c(lsh[1,1], lsh[1,1] + dx*seq(n-1), lsh[3,1]);
  xdn = c(lsh[2,1], lsh[2,1] + dx*seq(n-1), lsh[4,1]);
  yup = c(lsh[1,2], lsh[1,2] + dy*seq(n-1), lsh[3,2]);
  ydn = c(lsh[2,2], lsh[2,2] + dy*seq(n-1), lsh[4,2]);
  # Plot:
  for(id in seq(n)) {
    lines(c(xup[id], xdn[id]), c(yup[id], ydn[id]), lwd=lwd, lty=lty, col=col);
  }
  invisible(cbind(xup, xdn, yup, ydn));
}

#############
#############

### Test:
plot.base = function(xlim=c(-2,10), ylim=c(-2,10), axt=c(2)) {
  mar = 0.25 + c(2,2,0,0); # TODO
  par.old = par(mar = mar);
  plot.new()
  plot.window(xlim=xlim, ylim=ylim)
  if( ! is.null(axt)) {
    lapply(axt, function(a) axis(a));
  }
  invisible(par.old);
}

##############

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


##############
### Example 2:

### Test: Banded lines
plot.base()
lineBanded(c(0,5), c(1, 8), lwd=2.5)
lineBanded(c(1,7), c(0, 5), lwd=2.5)
lineBanded(c(0,6), c(6, 0), lwd=2.5, col="green")


# TODO:
# - verification of formulas;
# - test cases for various types of base-lines and points;
# - base line:
#  -- origin: (0, 0), (0, 1), (0, 2), (0, -2), ...;
#  -- slope: ascending, descending, vertical, horizontal,
#     slightly perturbed vertical or horizontal;
# - separate source file with the various tests;


###############

### Derivation:

### Reflection:
# - slope.orthogonal = - 1 / slope;
# - intersection between base-line and reflexion line:
# y.int = (x.int - x[1])*slope + y[1] # Initial Line 
# y.int = - (x.int - p[1])/slope + p[2]; # p[1] = x; p[2] = y; #still can't debate about it
# =>
# x.int = (x[1]*slope + p[1]/slope - y[1] + p[2]) / (slope + 1/slope); 
# y.int = - (x.int - p[1])/slope + p[2];


### Shift:
# y.sh = - (x.sh - x[1]) / slope + y[1];
# (x.sh - x[1])^2 + (y.sh - y[1])^2 = d^2
# =>
sl.o = - 1 / slope;
(sl.o^2 + 1)*x.sh^2 - 2*x1*(sl.o^2 + 1)*x.sh + x1^2*(sl.o^2 + 1) - d^2 # = 0


#################
#################

arrowDH = function(x, y, lwd=1, h.lwd=lwd, col="red", asD=FALSE) {
  slope = compute_slope(x, y);
  if(asD) {
    l = shift(x, y, d = 0.2, slope=slope);
    lines(l[c(1,3), 1], l[c(1,3), 2], lwd=lwd, col=col);
    lines(l[c(2,4), 1], l[c(2,4), 2], lwd=lwd, col=col);
  } else {
    lines(x, y, lwd=lwd, col=col);
  }
  ### Head
  # Shift point along line:
  p = shiftH(c(x[2], y[2]), x, y, d=-1)
  pV = shift(p, slope=slope);
  lines(c(pV[1,1], x[2], pV[2,1]),
        c(pV[1,2], y[2], pV[2,2]), lwd=h.lwd, col=col)
  # Double Arrow
  p = shiftH(c(x[2], y[2]), x, y, d = -1 - 0.2)
  pV = shift(p, slope=slope);
  pVV = shiftH(c(x[2], y[2]), x, y, d = 0 - 0.2)
  lines(c(pV[1,1], pVV[1,1], pV[2,1]),
        c(pV[1,2], pVV[1,2], pV[2,2]), lwd=h.lwd, col=col);
  return(); # TODO
}
arrowInvH = function(x, y, lwd=1, h.lwd=lwd, col="red") {
  slope = compute_slope(x, y);
  lines(x, y, lwd=lwd, col=col);
  ### Head
  # Shift point along line:
  p = shiftH(c(x[2], y[2]), slope=slope, d = 1)
  pV = shift(p, slope=slope, d=1);
  lines(c(pV[1,1], x[2], pV[2,1]),
        c(pV[1,2], y[2], pV[2,2]), lwd=h.lwd, col=col)
  # Stop Arrow
  pV = shift(c(x[2], y[2]), slope=slope, d = 1);
  lines(c(pV[1,1], pV[2,1]),
        c(pV[1,2], pV[2,2]), lwd=h.lwd, col=col);
  return(); # TODO
}

###
plot.base()
x = c(0,10); y = c(1, 5);
arrowDH(x, y, lwd=2, asD=TRUE);

x = c(0, 6); y = c(1, 6);
arrowInvH(x, y, lwd=2);

