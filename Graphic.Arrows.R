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


### Simple ArrowHead
# (x, y) = tip of the ArrowHead;
arrHS = function(x, y, slope, d=-1, dV=c(d, -d)) {
  p = if(d == 0) matrix(c(x, y), nrow=1, ncol=2)
  else shiftPoint(c(x, y), slope=slope, d = d);
  pV = shiftLine(p, slope=slope, d = dV);
  arrHead = list(
    x = c(pV[1,1], x, pV[2,1]),
    y = c(pV[1,2], y, pV[2,2]));
  return(arrHead);
}
# Double Lined Head
arrowDH = function(x, y, d=0.2, lwd=1, d.head=-1, h.lwd=lwd, col="red", asD=FALSE) {
  slope = compute_slope(x, y);
  xylist = function(x, y) list(list(x=x, y=y));
  ### ArrowTail
  if(asD) {
    arrow = shiftLine(x, y, d = c(d, -d), slope=slope);
    arrow = list(arrow, lwd=lwd);
  } else {
    arrow = xylist(x, y);
    arrow = c(arrow, lwd = lwd);
  }
  ### Head
  # Shift point along line:
  ahead = list(arrHS(x[2], y[2], slope=slope, d = d.head));
  # Double Arrow
  p2 = shiftPoint(c(x[2], y[2]), slope=slope, d = - d);
  ahead2 = list(arrHS(p2[1], p2[2], slope=slope, d = d.head));
  ahead  = c(ahead, ahead2, lwd = h.lwd);
  ### Full Arrow
  lst = list(Arrow=arrow, Head=ahead);
  class(lst) = c("arrow", "list");
  # Plot lines:
  print("Finished")
  lines(lst, col=col);
  invisible(lst);
}

### Other:
# TODO: fix & clean-up code;
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
