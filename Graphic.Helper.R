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
		if(length(x) < 2 || length(y) < 2)
			stop("The base-line requires 2 points!");
		# TODO: handle if more than 2 points!
		slope = compute_slope(x,y) # (y[[2]] - y[[1]]) / (x[[2]] - x[[1]]);
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
		if(length(x) < 2 || length(y) < 2)
			stop("The base-line requires 2 points!");
		# TODO: handle if more than 2 points!
		slope = compute_slope(x,y) # (y[[2]] - y[[1]]) / (x[[2]] - x[[1]]);
	}
	if(length(p) < 2) stop("Point needs both x & y coordinates!");
	slope.sqrt = 1 / sqrt(slope^2 + 1);
	dx = d * slope.sqrt;
	dy = dx * slope;
	xsh = p[[1]] + dx;
	ysh = p[[2]] + dy;
	return(cbind(x=xsh, y=ysh));
}


#######################

