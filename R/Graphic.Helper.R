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


### Initial Ideas


####################

### Helper Functions

# - reflect;
# - shift;
# - plot;

compute_slope = function(x, y) {
  if(length(x) < 2 || length(y) < 2) {
    stop("The base-line requires 2 points!");
    # TODO: check if it works properly.
  }

  if(length(x) == 2){
    slope = (y[[2]] - y[[1]]) / (x[[2]] - x[[1]]);
  }
  else{
    slope = diff(y) / diff(x);
  }
  return(slope);
}


### Plot:
lines.list = function(x, y, lwd=NULL, ...) {
  sapply(x, function(lst) {
    lwd = if(is.null(lwd)) {
      if(is.null(lst$lwd)) 1 else lst$lwd;
    } else lwd;
    lines(lst$x, lst$y, lwd=lwd, ...);
  });
}

### Base function
lines.object.base = function(x, lwd, col=1, ...) {
  # do NOT overwrite user-value;
  if(is.null(lwd)) {
    lwd = if(is.null(x$lwd)) 1 else x$lwd;
  }
  x$lwd = NULL;
  if(length(x) == 1 && inherits(x[[1]], "data.frame")) {
    x = split(x[[1]][, c("x", "y")], x[[1]]$id);
  }
  basef = function(lst, ...) {
    if(inherits(lst, "circle")) {
      shape::plotellipse(rx = lst$r, ry = lst$r, mid = lst$center,
                         col=col, lwd=lwd, ...);
    } else if(inherits(lst, "polygon")) {
      col0 = lst$col;
      col  = if(is.null(col0)) col else col0;
      polygon(lst$x, lst$y, col=col);
    } else lines(lst$x, lst$y, lwd=lwd, col=col, ...);
  }
  lapply(x, basef, ...);
  invisible();
}

### list(Tail=list(...), Head=list(...))
lines.arrow = function(x, lwd=NULL, col=1, ...) {
  ### ArrowTail
  arrow = x[[1]];
  lines.object.base(arrow, lwd=lwd, col=col, ...)
  ### ArrowHead
  ahead = x[[2]];
  lines.object.base(ahead, lwd=lwd, col=col, ...);
  #
  invisible();
}

### Chemistry
lines.chemistry = function(x, lwd=NULL, col=1, ...) {
  lines.object.base(x, lwd=lwd, col=col, ...)
  invisible();
}

### Reflect Point across line
# p = c(x, y) # the point to reflect;
reflect = function(x, y, p, slope=NULL) {
  if(is.null(slope)) {
    if(length(x) < 2 || length(y) < 2)
      stop("The base-line requires 2 points!");
    # TODO: handle if more than 2 points!
    slope = compute_slope(x,y) # (y[[2]] - y[[1]]) / (x[[2]] - x[[1]]);
  }

  if(slope == 0) {
    # Horizontal Line
    return(c(p[1], 2*y[1]-p[2]))
  } else if(x[[1]] == x[[2]]) {
    # Vertical Line
    return(c(2*x[1]-p[1], p[2])) # De ce?
    # TODO: distanta de la punct la dreapta + punctul initial ?
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

### Shift Line
# d = distance to shift (translate);
shiftLine = function(x, y, d=1, slope=NULL, scale=1) {
  if(is.null(slope)) {
    if(length(x) < 2 || length(y) < 2)
      stop("The base-line requires 2 points!");
    # TODO: handle if more than 2 points!
    slope = compute_slope(x,y);
  } else {
    if(missing(y)) {
      # both coordinates encoded using parameter x;
      y = x[2]; x = x[1];
    }
  }
  ### Vertical Line
  if(abs(slope) == Inf) {
    if(length(d) == 1) {
      r = data.frame(x = x + d, y = y);
    } else {
      r = lapply(seq(along=d), function(id) {
        data.frame(x = x + d[id], y = y, id=id);
      })
      r = do.call(rbind, r);
    }
    return(r)
  }
  ### Horizontal Line
  if(slope == 0) {
    d = d * scale;
    if(length(d) == 1) {
      r = data.frame(x = x, y = y + d);
    } else {
      r = lapply(seq(along=d), function(id) {
        data.frame(x = x, y = y + d[id], id=id);
      })
      r = do.call(rbind, r);
    }
    return(r)
  }
  ### Oblique Line
  sl.orto = - 1 / slope;
  sl2 = sqrt(sl.orto^2 + 1/scale^2);
  sl.orto = sl.orto * scale;
  # shift Start- & End-points:
  shift.f = function(x, y, id) {
    delta = d[id] / sl2;
    x.sh  = x + delta / scale;
    y.sh  = delta*sl.orto + y;
    data.frame(x=x.sh, y=y.sh, id=id);
  }
  rez = lapply(seq(length(d)), function(id) shift.f(x, y, id))
  rez = data.frame(do.call(rbind, rez));
  return(rez);
}

# shift point p along line defined by (x, y);
# d = distance;
shiftPoint = function(p, x, y, d=1, slope=NULL, scale=1) {
  if(is.null(slope)) {
    if(length(x) < 2 || length(y) < 2)
      stop("The base-line requires 2 points!");
    # TODO: handle if more than 2 points!
    slope = compute_slope(x,y);
  }
  if(length(p) < 2) stop("Point needs both x & y coordinates!");
  # V line: if(x[1] == x[2]) {
  if(abs(slope) == Inf) {
    r = cbind(x = p[1], y = p[2] + d);
    return(r)
  }
  slope.sqrt = 1 / sqrt(slope^2 + scale^2);
  dx = d * slope.sqrt;
  dy = dx * slope;
  xsh = p[[1]] + dx;
  ysh = p[[2]] + dy;
  return(cbind(x=xsh, y=ysh));
}

#### Helper Functions for Circles ####

solveCircleIntersection = function(mid1, mid2, r1, r2, digits=4, debug=TRUE) {
  xp = mid1[[1]]; yp = mid1[[2]];
  xc = mid2[[1]]; yc = mid2[[2]];
  d  = r1; r = r2;
  xs = xp + xc;
  xd = xp - xc; xd2 = xs*xd;
  yd = yp - yc;
  # P2:
  a  = 4*(xd^2 + yd^2);
  b1 = - 4*(xs^3 - 4*xp*xc*xs + yd^2*xs + (r^2 - d^2)*xd);
  b0 = d^4 + r^4 + xc^4 + xp^4 + yp^4 + yc^4 - 4*yp^3*yc - 4*yp*yc^3 + 6*yp^2*yc^2 +
    + 2*xp^2*yp^2 + 2*xp^2*yc^2 + 2*xc^2*yp^2 + 2*xc^2*yc^2 - 2*xp^2*xc^2 +
    + 2*(r^2 - d^2)*xd2 - 2*(r^2 + d^2)*yd^2 +
    - 4*yp*yc*xp^2 - 4*yp*yc*xc^2 - 2*d^2*r^2;
  # Det:
  D = b1^2 - 4*a*b0;
  if(debug) print(c(a, b1, b0, D));
  if(round(D, digits) == 0) D = 0;
  if(D < 0) return(NULL);
  D = sqrt(D);
  # Sol:
  x = ( - b1 + c(-D, D)) / (2*a);
  y =  yp^2 - yc^2 + xd2 - 2*xd*x + r^2 - d^2;
  div = 2*yd; # TODO: yp == yc;
  y = y / div;
  sol = data.frame(x=x, y=y);
  return(sol);
}

### Bio-Shapes
lines.bioshape = function(x, lwd=NULL, col=1, ...) {
  lines.object.base(x, lwd=lwd, col=col, ...)
  invisible();
}


#######################

