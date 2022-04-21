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

### Arrow Tail:
arrowTail = function(x, y, d.lines, lwd=1, slope=NULL) {
  if(is.null(slope)) slope = compute_slope(x, y);
  if(d.lines != 0) {
    arrTail = shiftLine(x, y, d = c(d.lines, -d.lines), slope=slope);
  } else {
    arrTail = list(x=x, y=y);
  }
  arrTail = list(arrTail, lwd=lwd);
  return(arrTail)
}

### Arrow Types
# dH = horizontal length of ">";
# dV = vertical height of ">";
# d = distance between each of ">>";


#### Arrow Simple ####
arrowSimpleH = function(x, y, d=-0.5, lwd=1, d.head=c(-d,d), d.lines=0, h.lwd=lwd, col="red") {
  slope = compute_slope(x, y);
  ### ArrowTail
  arrow = arrowTail(x, y, d.lines=d.lines, lwd=lwd, slope=slope);
  ### Head
  ahead  = list(arrowSimpleHead(x[2], y[2], slope=slope, d=d, dV = d.head), lwd = h.lwd);
  ### Full Arrow
  lst = list(Arrow=arrow, Head=ahead);
  class(lst) = c("arrow", "list");
  # Plot lines:
  print("Finished")
  lines(lst, col=col);
  invisible(lst);
}

#### Double Lined Arrow ####
arrowDH = function(x, y, d=0.2, lwd=1, d.head=-1, d.lines=0, h.lwd=lwd, col="red") {
  slope = compute_slope(x, y);
  ### ArrowTail
  arrow = arrowTail(x, y, d.lines=d.lines, lwd=lwd, slope=slope);
  ### Head
  arrHead = arrowDoubleHead(x[2], y[2], slope=slope, d=d, dH=d.head);
  arrHead$lwd = h.lwd;
  ### Full Arrow
  lst = list(Arrow=arrow, Head=arrHead);
  class(lst) = c("arrow", "list");
  # Plot lines:
  print("Finished")
  lines(lst, col=col);
  invisible(lst);
}

#### Arrow Diamond ####
arrowDiamondH = function(x, y, d=0.2, lwd=1, d.head=-1, d.lines=0, h.lwd=lwd, col="red") {
  slope = compute_slope(x, y);
  ### ArrowTail
  arrow = arrowTail(x, y, d.lines=d.lines, lwd=lwd, slope=slope);
  ### Head
  ahead  = list(arrowSimpleHead(x[2], y[2], slope=slope, d=d), lwd = h.lwd);
  ### Full Arrow
  lst = list(Arrow=arrow, Head=ahead);
  class(lst) = c("arrow", "list");
  # Plot lines:
  print("Finished")
  lines(lst, col=col);
  invisible(lst);
}

#### Arrow X ####
arrowXH = function(x, y, d=0.5, lwd=1, d.head=c(-d, d), d.lines=0, h.lwd=lwd, col="red") {
  slope = compute_slope(x, y);
  ### Head
  arrHead = arrowXHead(x[2], y[2], slope=slope, d = - d, dV = d.head);
  ahead  = list(arrHead, lwd = h.lwd);
  midpoint = attr(arrHead, "Mid")
  ### ArrowTail
  x[2] = midpoint[1]
  y[2] = midpoint[2]
  arrow = arrowTail(x, y, d.lines=d.lines, lwd=lwd, slope=slope);
  ### Full Arrow
  lst = list(Arrow=arrow, Head=ahead);
  class(lst) = c("arrow", "list");
  # Plot lines:
  print("Finished")
  lines(lst, col=col);
  invisible(lst);
}

#### Arrow T ####
arrowTH = function(x, y, d=0.2, lwd=1, d.head=-1, d.lines=0, h.lwd=lwd, col="red") {
  slope = compute_slope(x, y);
  ### ArrowTail
  arrow = arrowTail(x, y, d.lines=d.lines, lwd=lwd, slope=slope);
  ### Head
  ahead  = list(arrowTHead(x[2], y[2], slope=slope, d=d), lwd = h.lwd);
  ### Full Arrow
  lst = list(Arrow=arrow, Head=ahead);
  class(lst) = c("arrow", "list");
  # Plot lines:
  print("Finished")
  lines(lst, col=col);
  invisible(lst);
}

#### Arrow Square ####
arrowSQ = function(x, y, d=0.2, lwd=1, d.head=-1, d.lines=0, h.lwd=lwd, col="red") {
  slope = compute_slope(x, y);
  ### Head
  arrHead = arrowSquareHead(x[2], y[2], slope=slope, d=d);
  ahead  = list(arrHead, lwd = h.lwd);
  ### ArrowTail
  arrow = arrowTail(x, y, d.lines=d.lines, lwd=lwd, slope=slope);
  ### Full Arrow
  lst = list(Arrow=arrow, Head=ahead);
  class(lst) = c("arrow", "list");
  # Plot lines:
  print("Finished")
  lines(lst, col=col);
  invisible(lst);
}

# n = number of sub-components;
# d = distance between each "> >";
# dH = horizontal shift (shiftPoint)
# dV = vertical shift (shiftLine) of each ">";
arrowMultiH = function(x, y, slope, n=1, lwd=1, d=0.25, h.lwd=lwd, d.head=c(-d, d), d.lines=0, col="red") {
  slope = compute_slope(x, y);
  ### ArrowTail
  arrow = arrowTail(x, y, d.lines=d.lines, lwd=lwd, slope=slope);
  ### Head
  arrHead = arrowNHead(x[2], y[2], slope=slope, n=n, d=d, dV=d.head);
  arrHead = c(arrHead, arrowNHead(x[2], y[2], slope=slope, n=n, d=d, dV=-d.head));
  arrHead$lwd = h.lwd;
  ### Full Arrow
  lst = list(Arrow=arrow, Head=arrHead);
  class(lst) = c("arrow", "list");
  # Plot lines:
  print("Finished")
  lines(lst, col=col);
  invisible(lst);
}

# Double Lined Inverted Head
arrowInv2H = function(x, y, d=0.25, lwd=1, d.head=c(-d, d), d.lines=0, h.lwd=lwd, col="red") {
  slope = compute_slope(x, y);
  ### Head
  arrHead = arrowDoubleInvertedHead(x[2], y[2], slope=slope, d=d, dV=d.head);
  midpoint = attr(arrHead, "Mid")
  arrHead$lwd = h.lwd;
  ### ArrowTail
  x[2] = midpoint[1]
  y[2] = midpoint[2]
  arrow = arrowTail(x, y, d.lines=d.lines, lwd=lwd, slope=slope);
  ### Full Arrow
  lst = list(Arrow=arrow, Head=arrHead);
  class(lst) = c("arrow", "list");
  # Plot lines:
  print("Finished")
  lines(lst, col=col);
  invisible(lst);
}


### Other:
arrowInvH = function(x, y, lwd=1, d=1, d.head=c(-d,d), d.lines=0, h.lwd=lwd, col="red") {
  slope = compute_slope(x, y);
  xylist = function(x, y) list(list(x=x, y=y));
  ### Full Arrow
  arrow = lines(x, y, lwd=lwd, col=col);
  p = shiftPoint(c(x[2], y[2]), slope=slope, d = d)
  pV = shiftLine(p, slope=slope, d=d.head);

  ### Head
  ahead = lines(c(pV[1,1], x[2], pV[2,1]),
                c(pV[1,2], y[2], pV[2,2]), lwd=h.lwd, col=col)
  # Putting them into the list
  lst = list(Arrow=arrow, Head=ahead);
  class(lst) = c("arrow");

  # Plot lines:
  print("Finished")
  lines(lst, col=col);
  invisible(); # TODO
}

#### Arrow Circle
arrowCircle = function(x, y, r=0.5, lwd=1, d.lines=0, h.lwd=lwd, col="red") {
  slope = compute_slope(x, y);
  ### Head
  arrHead = arrowCircleHead(x[2], y[2], slope=slope, r=r);
  start = attr(arrHead, "start")
  arrHead$lwd = h.lwd;
  ### ArrowTail
  x[2] = start[1]
  y[2] = start[2]
  arrow = arrowTail(x, y, d.lines=d.lines, lwd=lwd, slope=slope);
  ### Full Arrow
  lst = list(Arrow=arrow, Head=arrHead);
  class(lst) = c("arrow", "list");
  # Plot lines:
  print("Finished")
  lines(lst, col=col);
  invisible(lst);
}

#### Arrow Solid Square
arrowSolidSQ = function(x, y, d=0.2, lwd=1, d.head=-1, d.lines=0, h.lwd=lwd, col="red") {
  slope = compute_slope(x, y);
  ### Head
  arrHead = arrowSquareHead(x[2], y[2], slope=slope, d=d);
  class(arrHead) = c("polygon", class(arrHead));
  ahead  = list(arrHead, lwd = h.lwd);
  ### ArrowTail
  arrow = arrowTail(x, y, d.lines=d.lines, lwd=lwd, slope=slope);
  ### Full Arrow
  lst = list(Arrow=arrow, Head=ahead);
  class(lst) = c("arrow", "list");
  # Plot lines:
  print("Finished")
  lines(lst, col=col);
  invisible(lst);
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

