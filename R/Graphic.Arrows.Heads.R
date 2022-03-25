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


### Functions to Generate ArrowHeads

#####################

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

# Diamond ArrowHead: ---<>
arrHD = function(x, y, slope, d=-1, dV=c(d, -d)) {
  if(length(d) > 2) stop("Only 2 values are supported for d!");
  d1 = d[[1]];
  d2 = if(length(d) == 1) 2*d else sum(d);
  # TODO: more than 2 values for dV;
  pV = arrHS(x, y, slope=slope, d=d1, dV=dV);
  p2 = shiftPoint(c(x, y), slope=slope, d = d2);
  arrHead = list(
    x = c(pV$x[1], p2[1,1], pV$x[3], x, pV$x[1]),
    y = c(pV$y[1], p2[1,2], pV$y[3], y, pV$y[1]));
  return(arrHead);
}

# X ArrowHead: ---X
arrHX = function(x, y, slope, d=-1, dV=c(d, -d)) {
  if(length(d) > 2) stop("Only 2 values are supported for d!");
  d2 = if(length(d) == 1) 2*d else sum(d);
  # TODO: more than 2 values for dV;
  pB1 = c(x[1], y[1]);
  pB2 = shiftPoint(c(x[1], y[1]), d=d2, slope=slope);
  p1 = shiftLine(pB1, d=dV, slope=slope);
  p2 = shiftLine(pB2, d=dV, slope=slope);
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
arrH2 = function(x, y, slope, d=-1, dH=-d, dV=c(dH, -dH)) {
  # Shift point along line:
  arrHead = list(arrHS(x, y, slope=slope, d = dH, dV = dV));
  # Double Arrow
  p2 = shiftPoint(c(x, y), slope=slope, d = - d);
  arrHead2 = list(arrHS(p2[1], p2[2], slope=slope, d = dH, dV = dV));
  arrHead  = c(arrHead, arrHead2);
  return(arrHead);
}

# N-Lined ArrowHead: --->>...> (n times)
arrHN = function(x, y, slope, n=1, d = 0.5, dH = - d, dV=c(dH, -dH)) {
  # Shift point along line:
  arrHead = list(arrHS(x, y, slope=slope, d = dH, dV = dV));
  if(n == 1) return(arrHead);
  # Double Arrow
  for(id in seq(n-1)) {
    p = shiftPoint(c(x, y), slope=slope, d = - id * d);
    arrowhead = list(arrHS(p[1], p[2], slope=slope, d = dH, dV = dV));
    arrHead  = c(arrHead, arrowhead);
  }
  return(arrHead);
}

# T ArrowHead: ---|
arrHT = function(x, y, slope, d=-1, dV=c(d, -d)) {
  p  = cbind(x, y);
  pV = shiftLine(p, slope=slope, d = dV);
  arrHead = list(
    x = c(pV[1,1], pV[2,1]),
    y = c(pV[1,2], pV[2,2]));
  return(arrHead);
}

# Square ArrowHead: |_|
arrSQH = function(x, y, slope, d=-1, dV=c(d, -d)) {
  if(length(d) > 2) stop("Only 2 values are supported for d!");
  d2 = if(length(d) == 1) 2*d else sum(d);
  # TODO: more than 2 values for dV;
  pB1 = c(x[1], y[1]);
  pB2 = shiftPoint(c(x[1], y[1]), d=d, slope=slope);
  p1 = shiftLine(pB1, d=dV, slope=slope);
  p2 = shiftLine(pB2, d=dV, slope=slope);
  arrHead = list(
    x = c(p1$x[2], p1$x[1], p2$x[1],  p2$x[2], p1$x[2]),
    y = c(p1$y[2], p1$y[1], p2$y[1],  p2$y[2], p1$y[2]));
  return(arrHead);
}

# Double Lined Inverted ArrowHead: ---<<
arrInv2H = function(x, y, slope, d=-1, dV=c(d, -d)) {
  # Shift point along line:
  dH = abs(dV[1]);
  # Head: 2nd "<" of "<<"
  p2 = shiftPoint(c(x, y), slope=slope, d = - dH);
  arrHead = list(arrHS(p2[1], p2[2], slope=slope, d = dH));
  # Head: 1st "<" of "<<"
  p2 = shiftPoint(c(x, y), slope=slope, d = - d - dH);
  arrHead2 = list(arrHS(p2[1], p2[2], slope=slope, d = dH));
  arrHead  = c(arrHead, arrHead2);
  midpoint = p2;
  attr(arrHead, "Mid") = midpoint;
  return(arrHead);
}

# Circle ArrowHead ---O
arrCircle = function(x, y, slope, r=0.5) {
  center = shiftPoint(c(x, y), slope = slope, d = -r)
  startP = shiftPoint(c(x, y), slope = slope, d = -2*r)
  lst = list(r=r, center=center);
  attr(lst, "class") = c("circle", class(lst));
  lst = list(lst);
  attr(lst, "start") = startP;
  return(lst)
}

