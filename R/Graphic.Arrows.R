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


### Functions to Generate Arrows

#####################

### Helper Functions

### Arrow function:
#' @export
arrow = function(x, y, type = "Simple", d=1, lwd=1, ...) {
  call = match.call();
  idType = match("type", names(call));
  if(is.na(idType)) {
    type = "Simple";
  } else {
    types = c("Simple", "Double", "Diamond",
              "Square", "Inverted", "T", "X", "N",
              "DoubleInverted", "Circle", "SolidSquare"); # de completat
    type = call[[idType]];
    type = pmatch(type, types);
    if(is.na(type)) stop("Invalid type!");
    call = call[ - idType];
    type = types[type];
  }
  # Function name:
  type = paste0("arrow", type);
  type = as.symbol(type);
  call[[1]] = type;
  eval(call)
}

### Arrow Tail:
#' @export
arrowTail = function(x, y, d.lines, lwd=1, slope=NULL) {
  if(is.null(slope)) slope = compute_slope(x, y);
  if(any(d.lines != 0)) {
    arrTail = shiftLine(x, y, d = d.lines, slope=slope);
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
# - for consistency: join = 0;
#' @export
arrowSimple = function(x, y, d=-0.5, lwd=1, d.head=c(-d,d), d.lines=0,
                       h.lwd=lwd, col="red", scale=1, join=0) {
  slope = compute_slope(x, y);
  ### Head
  ahead = list(arrowHeadSimple(x[2], y[2], slope=slope, d=d, dV = d.head, scale=scale),
               lwd = h.lwd);
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

#### Double Lined Arrow ####
#' @export
arrowDouble = function(x, y, d=-0.5, lwd=1, d.head=-1, dV=c(-d.head, d.head), d.lines=0,
                       h.lwd=lwd, col="red", scale=1, join=0) {
  if(join > 2) stop("Unsupported value for join!");
  slope = compute_slope(x, y);
  ### Head
  arrHead = arrowHeadDouble(x[2], y[2], slope=slope, d=d, dH=d.head, dV=dV, scale=scale);
  arrHead$lwd = h.lwd;
  ### ArrowTail
  if(join == 1) {
    x[2] = arrHead[[2]]$x[2];
    y[2] = arrHead[[2]]$y[2];
  }
  arrow = arrowTail(x, y, d.lines=d.lines, lwd=lwd, slope=slope);
  ### Full Arrow
  lst = list(Arrow=arrow, Head=arrHead);
  class(lst) = c("arrow", "list");
  # Plot lines:
  lines(lst, col=col);
  invisible(lst);
}


#### Arrow T ####
#' @export
arrowT = function(x, y, d=0.2, lwd=1, d.head=c(d, -d), d.lines=0, h.lwd=lwd,
                  col="red", scale=1, join=0, lty=1) {
  slope = compute_slope(x, y);
  ### Head
  if(is.list(d.head)) {
    ahead = lapply(seq(length(d.head)), function(id) {
      arrowHeadT(x[2], y[2], slope=slope, dV=d.head[[id]], scale=scale)
    });
    ahead$lwd = h.lwd;
  } else {
    ahead = list(arrowHeadT(x[2], y[2], slope=slope, dV=d.head, scale=scale),
                 lwd = h.lwd);
  }
  ### ArrowTail
  arrow = arrowTail(x, y, d.lines=d.lines, lwd=lwd, slope=slope);
  ### Full Arrow
  lst = list(Arrow=arrow, Head=ahead);
  class(lst) = c("arrow", "list");
  # Plot lines:
  lines(lst, col=col, lty=lty);
  invisible(lst);
}

#### Arrow for Measurements ####
#' @export
arrowMeasure = function(x, y, d=-0.5, lwd=1, d.head=c(-d,d), dT=d.head, d.lines=0,
                        h.lwd=lwd, col="red", scale=1, join=0) {
  slope = compute_slope(x, y);
  ### Head
  arrHead = arrowHeadMeasure(x[2], y[2], slope=slope, d=d, dV = d.head, dT=dT, scale=scale);
  arrHead$lwd = h.lwd;
  ### ArrowTail
  arrow = arrowTail(x, y, d.lines=d.lines, lwd=lwd, slope=slope);
  ### Full Arrow
  lst = list(Arrow=arrow, Head=arrHead);
  class(lst) = c("arrow", "list");
  # Plot lines:
  lines(lst, col=col);
  invisible(lst);
}

# n = number of sub-components;
# d = distance between each "> >";
# dH = horizontal shift (shiftPoint)
# dV = vertical shift (shiftLine) of each ">";
#' @export
arrowN = function(x, y, n=1, d=-0.5, lwd=1, h.lwd=lwd, d.head=c(-d, d), d.lines=0,
                  col="red", scale=1, join=0) {
  if(join > n) stop("Unsupported value for join!");
  slope = compute_slope(x, y);
  ### Head
  arrHead = arrowHeadN(x[2], y[2], slope=slope, n=n, d=d, dV=d.head, scale=scale);
  arrHead$lwd = h.lwd;
  ### ArrowTail
  if(join == 0) {
    join = 1;
  }
  x[2] = arrHead[[join]]$x[2];
  y[2] = arrHead[[join]]$y[2];
  arrow = arrowTail(x, y, d.lines=d.lines, lwd=lwd, slope=slope);
  ### Full Arrow
  lst = list(Arrow=arrow, Head=arrHead);
  class(lst) = c("arrow", "list");
  # Plot lines:
  lines(lst, col=col);
  invisible(lst);
}

### Double Lined Inverted Head
# dH = abs(d) ensures always inverted!
#' @export
arrowDoubleInverted = function(x, y, d=-0.25, lwd=1, dH=abs(d), d.head=c(-d, d), d.lines=0,
                               h.lwd=lwd, col="red", scale=1, join=0) {
  if(join > 2) stop("Unsupported value for join!");
  slope = compute_slope(x, y);
  ### Head
  arrHead = arrowHeadDoubleInverted(x[2], y[2], slope=slope, d=d, dH=dH, dV=d.head, scale=scale);
  midpoint = attr(arrHead, "Mid")
  arrHead$lwd = h.lwd;
  ### ArrowTail
  if(join <= 1) {
    midpoint = midpoint[[2]];
  } else {
    midpoint = midpoint[[1]];
  }
  x[2] = midpoint[1]
  y[2] = midpoint[2]
  arrow = arrowTail(x, y, d.lines=d.lines, lwd=lwd, slope=slope);
  ### Full Arrow
  lst = list(Arrow=arrow, Head=arrHead);
  class(lst) = c("arrow", "list");
  # Plot lines:
  lines(lst, col=col);
  invisible(lst);
}


### Other: ---<
#' @export
arrowInverted = function(x, y, d=1, lwd=1, d.head=c(-d,d),
                         d.lines=0, h.lwd=lwd, col="red", scale=1, join=0) {
  slope = compute_slope(x, y);
  ### Head
  p = shiftPoint(c(x[2], y[2]), slope=slope, d = -d, scale=scale)
  pV = shiftLine(c(x[2], y[2]), slope=slope, d=d.head, scale=scale);
  ahead = lines(c(pV[1,1], p[1], pV[2,1]),
                c(pV[1,2], p[2], pV[2,2]), lwd=h.lwd, col=col);
  ### Arrow Tail
  x[2] = p[1];
  y[2] = p[2];
  arrow = lines(x, y, lwd=lwd, col=col);
  ### Full Arrow
  lst = list(Arrow=arrow, Head=ahead);
  class(lst) = c("arrow");
  
  # Plot lines:
  lines(lst, col=col);
  invisible(lst);
}


#### Arrow Diamond ####
#' @export
arrowDiamond = function(x, y, d=0.2, lwd=1, d.head=-1, d.lines=0, h.lwd=lwd, col="red", scale=1, join=0) {
  if(join > 2) stop("Unsupported value for join!");
  slope = compute_slope(x, y);
  ### Head
  ahead  = list(arrowHeadDiamond(x[2], y[2], slope=slope, d=d, scale=scale), lwd = h.lwd);
  ### ArrowTail
  if(join == 0 || join == 1) {
    x[2] = ahead[[1]]$x[2];
    y[2] = ahead[[1]]$y[2];
  }
  arrow = arrowTail(x, y, d.lines=d.lines, lwd=lwd, slope=slope);
  ### Full Arrow
  lst = list(Arrow=arrow, Head=ahead);
  class(lst) = c("arrow", "list");
  # Plot lines:
  lines(lst, col=col);
  invisible(lst);
}

#### Arrow X ####
# - for consistency: join = 0;
#' @export
arrowX = function(x, y, d=0.5, lwd=1, d.head=c(-d, d), d.lines=0,
                  h.lwd=lwd, col="red", scale=1, join=0) {
  slope = compute_slope(x, y);
  ### Head
  arrHead = arrowHeadX(x[2], y[2], slope=slope, d = - d, dV = d.head, scale=scale);
  ahead   = list(arrHead, lwd = h.lwd);
  midpoint = attr(arrHead, "Mid")
  ### ArrowTail
  x[2] = midpoint[1]
  y[2] = midpoint[2]
  arrow = arrowTail(x, y, d.lines=d.lines, lwd=lwd, slope=slope);
  ### Full Arrow
  lst = list(Arrow=arrow, Head=ahead);
  class(lst) = c("arrow", "list");
  # Plot lines:
  lines(lst, col=col);
  invisible(lst);
}


#### Arrow Circle
#' @export
arrowCircle = function(x, y, r=0.5, lwd=1, d.lines=0,
                       h.lwd=lwd, col="red", fill=NULL, scale=1, join=0) {
  if(join > 3) stop("Unsupported value for join!");
  slope = compute_slope(x, y);
  ### Head
  arrHead = arrowHeadCircle(x[2], y[2], slope=slope, r=r, scale=scale);
  mid = attr(arrHead, "start")
  arrHead$lwd = h.lwd;
  arrHead[[1]]$fill = fill;
  ### ArrowTail
  if(join < 3) {
    mid = if(join == 2) mid[[2]] else mid[[1]];
    x[2] = mid[1];
    y[2] = mid[2];
  }
  arrow = arrowTail(x, y, d.lines=d.lines, lwd=lwd, slope=slope);
  ### Full Arrow
  lst = list(Arrow=arrow, Head=arrHead);
  class(lst) = c("arrow", "list");
  # Plot lines:
  lines(lst, col=col);
  invisible(lst);
}


#### Arrow Square ####
# default: fill = NULL;
#' @export
arrowSquare = function(x, y, d=-0.5, lwd=1, d.head=c(d, -d)/2, d.lines=0,
                       h.lwd=lwd, col="red", fill=NULL, scale=1, join=0) {
  if(join > 2) stop("Unsupported value for join!");
  slope = compute_slope(x, y);
  ### Head
  arrHead = arrowHeadSquare(x[2], y[2], slope=slope, d=d, dV=d.head, scale=scale);
  if( ! is.null(fill)) {
    arrHead$fill = fill;
    class(arrHead) = c("polygon", class(arrHead));
  }
  ahead   = list(arrHead, lwd = h.lwd);
  ### ArrowTail
  if((join < 2 && d <= 0) || (join == 2 && d > 0)) {
    mid  = attr(arrHead, "Mid");
    x[2] = mid[1]; y[2] = mid[2];
  }
  arrow = arrowTail(x, y, d.lines=d.lines, lwd=lwd, slope=slope);
  ### Full Arrow
  lst = list(Arrow=arrow, Head=ahead);
  class(lst) = c("arrow", "list");
  # Plot lines:
  lines(lst, col=col);
  invisible(lst);
}

#### Arrow Solid Square
#' @export
arrowSolidSquare = function(x, y, d=-0.5, lwd=1, d.head=c(d, -d)/2, d.lines=0,
                            h.lwd=lwd, col="red", fill=col, scale=1, join=0) {
  if(join > 2) stop("Unsupported value for join!");
  slope = compute_slope(x, y);
  ### Head
  arrHead = arrowHeadSquare(x[2], y[2], slope=slope, d=d, dV=d.head, scale=scale);
  arrHead$fill = fill;
  class(arrHead) = c("polygon", class(arrHead));
  ahead = list(arrHead, lwd = h.lwd);
  ### ArrowTail
  if(join < 2) {
    mid  = attr(arrHead, "Mid");
    x[2] = mid[1]; y[2] = mid[2];
  }
  arrow = arrowTail(x, y, d.lines=d.lines, lwd=lwd, slope=slope);
  ### Full Arrow
  lst = list(Arrow=arrow, Head=ahead);
  class(lst) = c("arrow", "list");
  # Plot lines:
  lines(lst, col=col);
  invisible(lst);
}


#### Arrow Triangle ####
#' @export
arrowTriangle = function(x, y, d=-0.5, lwd=1, d.head=c(-d,d), d.lines=0,
                         h.lwd=lwd, col="red", scale=1, join=0) {
  if(join > 2) stop("Unsupported value for join!");
  slope = compute_slope(x, y);
  ### Head
  arrHead = arrowHeadTriangle(x[2], y[2], slope=slope, d=d, dV = d.head, scale=scale);
  mid     = attr(arrHead, "Mid");
  arrHead = list(arrHead, lwd=h.lwd);
  ### ArrowTail
  if(join == 0 || join == 1) {
    x[2] = mid[1]; y[2] = mid[2];
  }
  arrow = arrowTail(x, y, d.lines=d.lines, lwd=lwd, slope=slope);
  ### Full Arrow
  lst = list(Arrow=arrow, Head=arrHead);
  class(lst) = c("arrow", "list");
  # Plot lines:
  lines(lst, col=col);
  invisible(lst);
}

### Generate complex lines

### Complex Lines:
# - just a simple Example;
# TODO: correct function
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