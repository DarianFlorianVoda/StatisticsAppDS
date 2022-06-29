###################
#
# Bachelor Thesis
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


##########################
#### Helper Functions ####

### Example 1:

# Base-Line
# Note: x = (x0, x1); y = (y0, y1);

plot.base()
x = c(0,10); y = c(1, 5);
lines(x, y, lwd=2, col="red")

### Shift point along line:
p = shiftPoint(c(0,1), x, y, d=1)
points(p, col="green")
# only as example:
p = shiftPoint(c(0,2), x, y, d=seq(1, 4, by=0.5))
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
l = shiftLine(x, y, d=2)
len = nrow(l) / 2;
sapply(seq(len), function(id) lines(l[c(id, id+len),1], l[c(id, id+len),2], col="orange"))

### Derivation:

### Reflection:
# - slope.orthogonal = - 1 / slope;
# - intersection between base-line and reflexion line:
# y.int = (x.int - x[1])*slope + y[1] # Initial Line
# y.int = - (x.int - p[1])/slope + p[2]; # p[1] = x; p[2] = y;
# =>
# x.int = (x[1]*slope + p[1]/slope - y[1] + p[2]) / (slope + 1/slope);
# y.int = - (x.int - p[1])/slope + p[2];


### Shift:
# x = c(0,10); y = c(1, 5);
# slope = compute_slope(x, y);
# y.sh = - (x.sh - x[1]) / slope + y[1];
# (x.sh - x[1])^2 + (y.sh - y[1])^2 = d^2
# =>
#sl.o = - 1 / slope;
#(sl.o^2 + 1)*x.sh^2 - 2*x1*(sl.o^2 + 1)*x.sh + x1^2*(sl.o^2 + 1) - d^2 # = 0


#################
#### Objects ####

all_objects = function(){

  # Plot
  plot.base(xlim=c(-1,10), ylim=c(-1,10))

  ### Row 1:

  ### Liposome
  testLiposome = function(lst, col="#48B000", col.line=1){
    testFilledCircle(lst[[1]], line = FALSE, add = TRUE, col=col)
    testFilledCircle(lst[[2]], line = FALSE, add = TRUE, col=col)
    lines.object.base(lst[3], lwd=1, col=col.line)
  }

  y = c(7, 9); yt = 6.7;
  lst = liposomes(c(30, 17), r=0.15, phi=c(0, pi/34), d=0.1, center=c(0.5, 8))
  testLiposome(lst)
  text(0.5, 6,
       "Liposome")


  ### Brush-Border Cell
  p1 = c(3, 6.5)
  cell = cellBrushBorder(p1, w=2, h=2)
  lines(cell)
  text(4, 6,
       "Brush-Border Cell")

  ### Smooth Muscles / Connective Tissue
  lst = cellSmooth(c(6, 9), c(8, 9), r=0.3)
  lines.object.base(lst, lwd=2)

  lst = cellSmooth(c(6, 9), c(6.5, 8), r=0.4)
  lines.object.base(lst, lwd=2)

  text(7.5, 6,
       "Smooth Muscles")


  ### Row 2:

  ### Ex 2: Vertical Object
  p1 = c(2.5,0.5); p2 = c(p1[1], 5);
  lst1 = helix(p1, p2)
  lst2 = helix(p1, p2, phi=-pi/2)
  #plot.base()
  lines(lst1, col="blue", lwd=2)
  lines(lst2, col="red", lwd=2)

  text(2.5, 0,
       "Helix/DNA")

  ### Ex 3: Vertical
  p1 = c(5.5,1.5); p2 = c(p1[1],4); dx = c(2.25,0);
  lst1 = spirals(p1, p2)
  lst2 = spirals(p2 + dx, p1 + dx)
  #plot.base()
  lines(lst1, col="purple", lwd=2)
  lines(lst2, col="orange", lwd=2)

  text(6.5, 0,
       "Spirals/Coils")


}
all_objects()


#### Arrows ####

all_arrows = function(){
  # Plot
  plot.base()

  ### Row 1:
  y = c(7, 9); yt = 6.7;
  abline(h = y[2], col="green")
  text(4, y[2] + 1,
       "Arrows with different ArrowHeads")

  # Simple ArrowHead
  x = c(-1, -1);
  d=-0.5;
  d.head=c(-0.5,0.5)
  a1 = arrowSimple(x, y, d=d, d.head=d.head, lwd=2);
  text(-1, yt,
       "Simple", cex = 0.75);

  # Double ArrowHead
  x = c(1, 1);
  d = -0.3;
  d.head=-0.5
  a2 = arrowDouble(x, y, d=d, d.head=d.head, lwd=2);
  text(1, yt,
       "Double", cex = 0.75);

  # Inverted ArrowHead
  x = c(3, 3);
  d = -0.5;
  d.head=c(-0.5,0.5)
  a3 = arrowInverted(x, y, d=d, d.head=d.head, lwd=2);
  text(3, yt,
       "Inverted", cex = 0.75);

  # Diamond ArrowHead
  x = c(5, 5);
  d.head = c(-0.5, 0.5);
  d = -0.5;
  arrowDiamond(x, y, d=d, d.head=d.head, lwd=2, join=0);
  text(5, yt,
       "Diamond", cex = 0.75);

  # T Shape ArrowHead
  x = c(7, 7);
  arrowT(x, y, d=-0.75, lwd=2);
  text(7, yt,
       "T Shape", cex = 0.75);

  # Measurement ArrowHead
  x = c(9.5, 9.5);
  arrowMeasure(x, y, d=-0.5, lwd=2);
  text(9.5, yt,
       "Measurement", cex = 0.70);


  ### Row 2
  y = c(3, 5); yt = 2.7;
  abline(h = y[2], col="green")

  # X Shape ArrowHead
  x = c(-1, -1);
  arrowX(x, y, d=0.5, lwd=2);
  text(-1, yt,
       "X Shape", cex = 0.70);

  # Square Shape ArrowHead
  x = c(1, 1);
  arrowSquare(x, y, d=-0.5, lwd=2);
  text(1, yt,
       "Square Shape", cex = 0.70);

  # Square Flag
  x = c(3, 3);
  arrowSquare(x, y, d=-0.5, d.head=c(0, 2*d), lwd=2);
  text(3.5, yt,
       "Square Flag", cex = 0.70);

  # Multiple-Lined ArrowHead
  n = 3; d = 0.5;
  x = c(6, 6);
  arrowN(x, y, n=n, d=d, lwd=2);
  text(6, yt,
       "Multiple-Lined", cex = 0.70);

  # Double Lined Inverted ArrowHead
  x = c(9, 9);
  arrowDoubleInverted(x, y, d=-0.3, lwd=2);
  text(9, yt,
       "Double-Lined Inverted", cex = 0.70);


  ### Row 3
  y = c(-1, 1); yt = -1.3;
  abline(h = y[2], col="green")

  # Solid Rectangle ArrowHead
  x = c(-1, -1);
  arrowSolidSquare(x, y, d=-0.5, lwd=2, col="darkred", fill="red");
  text(-1, yt,
       "Solid Rectangle", cex = 0.70);

  # Triangle ArrowHead
  x = c(1, 1);
  d = -0.5;
  a1 = arrowTriangle(x, y, d=d, lwd=2);
  text(1, yt,
       "Triangle", cex = 0.70);

  # Solid Circle ArrowHead #####
  x = c(3, 3);
  arrowCircle(x, y, r=0.5, lwd=2, fill="#FFB0A0");
  text(3, yt,
       "Solid Circle", cex = 0.70);

  # Simple Circle ArrowHead #####
  x = c(5, 5);
  arrowCircle(x, y, r=0.5, lwd=2);
  text(5, yt,
       "Simple Circle", cex = 0.70);
}

all_arrows()


#### Liposome Measured ####

measure_liposome = function() {

  # Liposome
  testLiposome = function(lst, col="#48B000", col.line=1){
    testFilledCircle(lst[[1]], line = FALSE, add = TRUE, col=col)
    testFilledCircle(lst[[2]], line = FALSE, add = TRUE, col=col)
    lines.object.base(lst[3], lwd=1, col=col.line)
  }
  plot.base(xlim=c(-10,10), ylim=c(-10,10))
  lst = liposomes(c(30, 17), r=0.5, phi=c(0, pi/34), d=0.2)
  testLiposome(lst)
  text(0, -6.5,
       "Liposome")

  # Measurement arrow r1
  x = c(6,6); y = c(-5, 5);
  measure(x, y, lwd=2)
  text(7, 0, srt=-90,
       "r=4.78")

  # Measurement arrow r2
  x = c(-2,2); y = c(0, 0);
  measure(x, y, lwd=2, d=c(-0.5, 0.5))
  text(0, 1,
       "r=2.72")
}

measure_liposome()


#### Description of a liposome ####
# d = Dimensions of ArrowHead;
definition_liposome = function(lbl = "Insert text", title = "Liposome",
                               lwd=2, d=-0.4, n = c(30, 17)) {
  if(length(lbl) == 1) lbl = rep(lbl, 3);
  # Liposome
  testLiposome = function(lst, col="#48B000", col.line=1){
    testFilledCircle(lst[[1]], line = FALSE, add = TRUE, col=col)
    testFilledCircle(lst[[2]], line = FALSE, add = TRUE, col=col)
    lines.object.base(lst[3], lwd=1, col=col.line)
  }
  lst = liposomes(n, r=0.5, phi=c(0, pi/(2*n[[2]])), d=0.2)
  testLiposome(lst)

  # Title
  if( ! is.null(title)) text(0, -6.5, title);

  # Left arrow
  # TODO: fix!
  a1 = arrowSimple(x=c(-2.7,-5), y=c(-4.6,-8), d=-d, lwd=lwd);
  text(-5.5, -9, lbl[[1]])

  # Right arrow
  a2 = arrowSimple(x=c(1.4, 5), y=c(-2.4,-7), d=d, lwd=lwd);
  text(5, -8, lbl[[2]])

  # Upper arrow
  a3 = arrowSimple(x=c(0.08,1), y=c(3.5,8), d=d, lwd=lwd);
  text(1, 9, lbl[[3]])
}


plot.base(xlim=c(-10,10), ylim=c(-10,10))
definition_liposome(
  c("Outer lipid layer", "Inner lipid layer", "Lipid bilayer"))


####################

#### All Arrows ####

all_arrows()


#### Banded lines ####
plot.base()
lineBanded(c(0,5), c(1, 8), lwd=2.5)
lineBanded(c(1,7), c(0, 5), lwd=2.5)
lineBanded(c(0,6), c(6, 0), lwd=2.5, col="green")
