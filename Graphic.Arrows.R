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


### Functions to Generate Arrows

#####################

### Helper Functions

### Arrows:

# Double Lined Head
arrowDH = function(x, y, d=0.2, lwd=1, h.lwd=lwd, col="red", asD=FALSE) {
  slope = compute_slope(x, y);
  xylist = function(x, y) list(list(x=x, y=y));
  ### Arrow
  if(asD) {
    l = shiftLine(x, y, d = c(d, -d), slope=slope);
    # TODO:
    arrow = xylist(l[c(1,3), 1], l[c(1,3), 2], lwd);
    arrow = c(arrow, xylist(l[c(2,4), 1], l[c(2,4), 2]), lwd=lwd);
  } else {
    arrow = xylist(x, y);
    arrow = c(arrow, lwd = lwd);
  }
  ### Head
  # Shift point along line:
  d.head = -1;
  p = shiftPoint(c(x[2], y[2]), x, y, d = d.head);
  print(p)
  pV = shiftLine(p, slope=slope); print(pV)
  ahead = xylist(
    x = c(pV[1,1], x[2], pV[2,1]),
    y = c(pV[1,2], y[2], pV[2,2]));
  # Double Arrow
  p = shiftPoint(c(x[2], y[2]), x, y, d = d.head - d);
  pV = shiftLine(p, slope=slope);
  pVV = shiftPoint(c(x[2], y[2]), x, y, d = 0 - d);
  ahead = c(ahead, xylist(
    x = c(pV[1,1], pVV[1,1], pV[2,1]),
    y = c(pV[1,2], pVV[1,2], pV[2,2])), lwd = h.lwd );
  ### Full Arrow
  lst = list(Arrow=arrow, Head=ahead);
  class(lst) = c("arrow", "list");
  # Plot lines:
  print("Finished")
  lines(lst, col=col);
  invisible(lst);
}
arrowInvH = function(x, y, lwd=1, h.lwd=lwd, col="red") {
  slope = compute_slope(x, y);
  lines(x, y, lwd=lwd, col=col);
  ### Head
  # Shift point along line:
  p = shiftPoint(c(x[2], y[2]), slope=slope, d = 1)
  pV = shiftLine(p, slope=slope, d=1);
  lines(c(pV[1,1], x[2], pV[2,1]),
        c(pV[1,2], y[2], pV[2,2]), lwd=h.lwd, col=col)
  # Stop Arrow
  pV = shiftLine(c(x[2], y[2]), slope=slope, d = 1);
  lines(c(pV[1,1], pV[2,1]),
        c(pV[1,2], pV[2,2]), lwd=h.lwd, col=col);
  invisible(); # TODO
}


### Generate complex lines

### Complex Lines:
# - just a simple Example;
lineBanded = function(x, y, w=0.1, delta=0.25, lwd=1.5, lty=1, n=NULL, col="black", slope=NULL) {
  if(is.null(slope)) {
    slope = compute_slope(x, y);
  }
  lsh = shiftLine(x, y, d=w, slope=slope);
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


### Test:
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
