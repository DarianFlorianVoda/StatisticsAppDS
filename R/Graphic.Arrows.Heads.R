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


### Functions to Generate arrowHeadSimple

#####################

### Simple ArrowHead: --->
# (x, y) = tip of the ArrowHead;
arrowHeadSimple = function(x, y, slope, d=-1, dV=c(-d, d), scale=1) {
  p = if(d == 0) matrix(c(x, y), nrow=1, ncol=2)
  else shiftPoint(c(x, y), slope=slope, d = d);
  pV = shiftLine(p, slope=slope, d = dV, scale=scale);
  arrHead = list(
    x = c(pV[1,1], x, pV[2,1]),
    y = c(pV[1,2], y, pV[2,2]));
  return(arrHead);
}

# Diamond ArrowHead: ---<>
arrowHeadDiamond = function(x, y, slope, d=-1, dV=c(d, -d), scale=1) {
  if(length(d) > 2) stop("Only 2 values are supported for d!");
  d1 = d[[1]];
  d2 = if(length(d) == 1) 2*d else sum(d);
  # TODO: more than 2 values for dV;
  pV = arrowHeadSimple(x, y, slope=slope, d=d1, dV=dV, scale=scale);
  p2 = shiftPoint(c(x, y), slope=slope, d = d2);
  arrHead = list(
    x = c(pV$x[1], p2[1,1], pV$x[3], x, pV$x[1]),
    y = c(pV$y[1], p2[1,2], pV$y[3], y, pV$y[1]));
  return(arrHead);
}

# X ArrowHead: ---X
arrowHeadX = function(x, y, slope, d=-1, dV=c(d, -d), scale=1) {
  if(length(d) > 2) stop("Only 2 values are supported for d!");
  d2 = if(length(d) == 1) 2*d else sum(d);
  # TODO: more than 2 values for dV;
  pB1 = c(x[1], y[1]);
  pB2 = shiftPoint(c(x[1], y[1]), d=d2, slope=slope);
  p1 = shiftLine(pB1, d=dV, slope=slope, scale=scale);
  p2 = shiftLine(pB2, d=dV, slope=slope, scale=scale);
  if(length(d) == 1) {
    # TODO: one pass;
    midpointX = (p1$x[2]+p2$x[1])/2;
    midpointY = (p1$y[2]+p2$y[1])/2;
    arrHead = list(
      x = c(p1$x[2], p2$x[1], midpointX, p2$x[2], p1$x[1]),
      y = c(p1$y[2], p2$y[1], midpointY, p2$y[2], p1$y[1]));
  } else {
    pM = shiftPoint(c(x[1], y[1]), d=d[1], slope=slope);
    midpointX = pM[1];
    midpointY = pM[2];
    arrHead = data.frame(
      x = c(p1$x[1], midpointX, p1$x[2], p2$x[1], midpointX, p2$x[2]),
      y = c(p1$y[1], midpointY, p1$y[2], p2$y[1], midpointY, p2$y[2]),
      id = c(1,1,1, 2,2,2));
  }
  arrHead = list(
    x = c(p1$x[2], p2$x[1], midpointX, p2$x[2], p1$x[1]),
    y = c(p1$y[2], p2$y[1], midpointY, p2$y[2], p1$y[1]));
  attr(arrHead, "Mid") = c(midpointX, midpointY)
  return(arrHead);
}

# Double Lined ArrowHead: --->>
# - a high-level helper function;
arrowHeadDouble = function(x, y, slope, d=-1, dH=-d, dV=c(dH, -dH), scale=1) {
  # Shift point along line:
  arrHead = list(arrowHeadSimple(x, y, slope=slope, d = dH, dV = dV, scale=scale));
  # Double Arrow
  p2 = shiftPoint(c(x, y), slope=slope, d = - d);
  arrHead2 = list(arrowHeadSimple(p2[1], p2[2], slope=slope, d = dH, dV = dV, scale=scale));
  arrHead  = c(arrHead, arrHead2);
  return(arrHead);
}

# N-Lined ArrowHead: --->>...> (n times)
arrowHeadN = function(x, y, slope, n=1, d = 0.5, dH = - d, dV=c(dH, -dH), scale=1) {
  # Shift point along line:
  arrHead = list(arrowHeadSimple(x, y, slope=slope, d = dH, dV = dV, scale=scale));
  if(n == 1) return(arrHead);
  # Double Arrow
  for(id in seq(n-1)) {
    p = shiftPoint(c(x, y), slope=slope, d = - id * d);
    arrowhead = list(arrowHeadSimple(p[1], p[2], slope=slope, d = dH, dV = dV, scale=scale));
    arrHead  = c(arrHead, arrowhead);
  }
  return(arrHead);
}

# T ArrowHead: ---|
arrowHeadT = function(x, y, slope, d=-1, dV=c(d, -d), scale=1) {
  p  = cbind(x, y);
  pV = shiftLine(p, slope=slope, d = dV, scale=scale);
  arrHead = list(
    x = c(pV[1,1], pV[2,1]),
    y = c(pV[1,2], pV[2,2]));
  return(arrHead);
}

# Square ArrowHead: |_|
arrowHeadSquare = function(x, y, slope, d=-1, dV=c(d, -d), scale=1) {
  if(length(d) > 2) stop("Only 2 values are supported for d!");
  d2 = if(length(d) == 1) 2*d else sum(d);
  # TODO: more than 2 values for dV;
  pB1 = c(x[1], y[1]);
  pB2 = shiftPoint(c(x[1], y[1]), d=d, slope=slope);
  p1 = shiftLine(pB1, d=dV, slope=slope, scale=scale);
  p2 = shiftLine(pB2, d=dV, slope=slope, scale=scale);
  arrHead = list(
    x = c(p1$x[2], p1$x[1], p2$x[1],  p2$x[2], p1$x[2]),
    y = c(p1$y[2], p1$y[1], p2$y[1],  p2$y[2], p1$y[2]));
  return(arrHead);
}

# Double Lined Inverted ArrowHead: ---<<
arrowHeadDoubleInverted = function(x, y, slope, d=-1, dV=c(d, -d), scale=1) {
  # Shift point along line:
  dH = abs(dV[1]);
  # Head: 2nd "<" of "<<"
  p2 = shiftPoint(c(x, y), slope=slope, d = - dH);
  arrHead = list(arrowHeadSimple(p2[1], p2[2], slope=slope, d = dH, scale=scale));
  # Head: 1st "<" of "<<"
  p2 = shiftPoint(c(x, y), slope=slope, d = - d - dH);
  arrHead2 = list(arrowHeadSimple(p2[1], p2[2], slope=slope, d = dH, scale=scale));
  arrHead  = c(arrHead, arrHead2);
  midpoint = p2;
  attr(arrHead, "Mid") = midpoint;
  return(arrHead);
}

# Circle ArrowHead ---O
arrowHeadCircle = function(x, y, slope, r=0.5, scale=1) {
  center = shiftPoint(c(x, y), slope = slope, d = -r)
  startP = shiftPoint(c(x, y), slope = slope, d = -2*r)
  lst = list(r=r, center=center);
  attr(lst, "class") = c("circle", class(lst));
  lst = list(lst);
  attr(lst, "start") = startP;
  return(lst)
}

