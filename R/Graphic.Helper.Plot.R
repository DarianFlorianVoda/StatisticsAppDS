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


### Create new plot window:
# Convenience function:
#' @export
plot.base = function(xlim=c(-2,10), ylim=c(-2,10), axt=c(1,2)) {
  mar = 0.25 + c(2,2,0,0); # TODO:
  par.old = par(mar = mar);
  plot.new()
  plot.window(xlim=xlim, ylim=ylim)
  if( ! is.null(axt)) {
    lapply(axt, function(a) axis(a));
  }
  invisible(par.old);
}

### Plot:
#' @export
lines.list = function(x, y, lwd=NULL, ...) {
  sapply(x, function(lst) {
    lwd = if(is.null(lwd)) {
      if(is.null(lst$lwd)) 1 else lst$lwd;
    } else lwd;
    lines(lst$x, lst$y, lwd=lwd, ...);
  });
}

### Base function
#' @export
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
#' @export
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
#' @export
lines.chemistry = function(x, lwd=NULL, col=1, ...) {
  lines.object.base(x, lwd=lwd, col=col, ...)
  invisible();
}

### Bio-Shapes
#' @export
lines.bioshape = function(x, lwd=NULL, col=1, ...) {
  lines.object.base(x, lwd=lwd, col=col, ...)
  invisible();
}


### Circles
# fill = fill colour;
# col  = colour of circle borders;
# col.line = colour of extra circle of radius R;
#' @export
lines.circles = function(x, R, fill="#B0B032", col=NULL, col.line="green", line=TRUE, ...) {
  xy = x;
  r = attr(xy, "r");
  if(is.null(r)) stop("Missing r!");
  x = xy$x; y = xy$y;
  n = length(x);
  if(length(r) < n) r = rep(r, n %/% length(r));
  #
  for(id in seq(n)) {
    if(is.null(fill)) {
      shape::filledcircle(r1=r[id], r2=0, mid = c(x[id], y[id]), ...)
    } else {
      shape::filledcircle(r1=r[id], r2=0, mid = c(x[id], y[id]), col=fill, ...);
    }
  }
  if(line) {
    R = if(is.null(R)) attr(xy, "R") else R;
    center = attr(xy, "center");
    shape::plotcircle(r=R, mid=center, lcol=col.line, col=NULL);
  }
}