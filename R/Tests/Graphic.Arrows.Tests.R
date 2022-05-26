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

aspect_ratio = 1.4
aspect_ratio_max = 2.0
#### Tests ####

Dsquare = function(xy, x0, y0) {
  sum((xy$x - x0)^2, (xy$y - y0)^2)
}

testArrow = function(h, d, dV=c(-d, d)){
  stopifnot(round(Dsquare(h, h$x[2], h$y[2]) - 2*d^2 - sum(dV^2), 8) == 0)
}





#### Diagonal, H & V Tests ####

##### Simple  ArrowHead ####

###### Test 1 ######
x = c(0, 6); y = c(1, 6);
d = -1;
plot.base()
a1 = arrowSimple(x, y, d=d, lwd=2);
a4 = arrowSimple(x, y, d=d, lwd=2, d.lines = c(-0.5,0.5));
a2 = arrowSimple(c(x[1], 5), c(y[1], y[1]), d=d, lwd=2);
a3 = arrowSimple(c(x[1], x[1]), c(y[1], 5), d=d, lwd=2);
# Head
h1 = a1$Head[[1]]
h2 = a2$Head[[1]]
h3 = a3$Head[[1]]
# - visual aids:
lines(h1$x[c(1,3)], h1$y[c(1,3)], col="green")
lines(h2$x[c(1,3)], h2$y[c(1,3)], col="green")
lines(h3$x[c(1,3)], h3$y[c(1,3)], col="green")

testArrow(h=h1, d=d)
testArrow(h=h2, d=d)
testArrow(h=h3, d=d)


###### Test 2 ######
# - narrow ArrowHead;
x = c(0, 6); y = c(1, 6) + 1;
d=-1.5; d.head = c(-0.5, 0.5);
plot.base()
a1 = arrowSimple(x, y, d=d, d.head=d.head, lwd=2);
a2 = arrowSimple(c(x[1], 5), c(y[1], y[1]), d=d, d.head=d.head, lwd=2);
a3 = arrowSimple(c(x[1], x[1]), c(y[1], 5), d=d, d.head=d.head, lwd=2);
# Head
h1 = a1$Head[[1]]
h2 = a2$Head[[1]]
h3 = a3$Head[[1]]
# - visual aids:
lines(h1$x[c(1,3)], h1$y[c(1,3)], col="green")
lines(h2$x[c(1,3)], h2$y[c(1,3)], col="green")
lines(h3$x[c(1,3)], h3$y[c(1,3)], col="green")
# Total length = (d^2 + dV[1]^2) + (d^2 + dV[2]^2)
testArrow(h=h1, d=d, dV=d.head)
testArrow(h=h2, d=d, dV=d.head)
testArrow(h=h3, d=d, dV=d.head)


###### Test 3 ######
x = c(0, 4); y = c(1, 60);
x2 = c(0, 6); y2 = c(1, 20);
d = -2; d.head = c(-1/2, 1/2);
scale = (100/12) * aspect_ratio_max;
plot.base(ylim = c(0,100))
a1 = arrowSimple(x, y, d=d, d.head=d.head, lwd=2, scale=scale);
a2 = arrowSimple(x2, y2, d=d, d.head=d.head, lwd=2, scale=scale);
a3 = arrowSimple(c(x[1], 5), c(y[1], y[1]), d=d, d.head=d.head, lwd=2, scale=scale);
a4 = arrowSimple(c(x[1], x[1]), c(y[1], 50), d=d, d.head=d.head, lwd=2, scale=scale);
# Head
h1 = a1$Head[[1]]
h2 = a2$Head[[1]]
h3 = a3$Head[[1]]
h4 = a4$Head[[1]]
# - visual aids:
lines(h1$x[c(1,3)], h1$y[c(1,3)], col="green")
lines(h2$x[c(1,3)], h2$y[c(1,3)], col="green")
lines(h3$x[c(1,3)], h3$y[c(1,3)], col="green")
lines(h4$x[c(1,3)], h4$y[c(1,3)], col="green")

##### Simple ArrowHead: Mixed ####
# - comparison between different dV (d.head argument);

###### Test 1 ######
x = c(0, 6); y = c(1, 6);
d = -1; d2 = -1.5;
plot.base()
a1 = arrowSimple(x, y, d=d, lwd=2);
a2 = arrowSimple(c(x[1], 5), c(y[1], y[1]), d=d, lwd=2);
a3 = arrowSimple(c(x[1], x[1]), c(y[1], 5), d=d, lwd=2);
# different dV:
a4 = arrowSimple(x, y+2, d=d2, d.head=c(-0.5, 0.5), lwd=2);
# Head
h1 = a1$Head[[1]]
h2 = a2$Head[[1]]
h3 = a3$Head[[1]]
h4 = a4$Head[[1]]
# - visual aids:
lines(h1$x[c(1,3)], h1$y[c(1,3)], col="green")
lines(h2$x[c(1,3)], h2$y[c(1,3)], col="green")
lines(h3$x[c(1,3)], h3$y[c(1,3)], col="green")
lines(h4$x[c(1,3)], h4$y[c(1,3)], col="green")
# Total length = (d^2 + dV[1]^2) + (d^2 + dV[2]^2)
stopifnot(round(Dsquare(h1, h1$x[2], h1$y[2]) - 2*d^2 - 2*d^2, 8) == 0)
stopifnot(round(Dsquare(h2, h2$x[2], h2$y[2]) - 2*d^2 - 2*d^2, 8) == 0)
stopifnot(round(Dsquare(h3, h3$x[2], h3$y[2]) - 2*d^2 - 2*d^2, 8) == 0)
stopifnot(round(Dsquare(h4, h4$x[2], h4$y[2]) - 2*d2^2 - 2*(0.5)^2, 8) == 0)


###### Test 2 ######
plot.base()
x = c(0, 6); y = c(1, 6) + 1;
d=-1.5; d.head = c(-0.5, 0.5);
a1 = arrowSimple(x, y, d=d, d.head=d.head, lwd=2);
a2 = arrowSimple(c(x[1], 5), c(y[1], y[1]), d=d, d.head=d.head, lwd=2);
a3 = arrowSimple(c(x[1], x[1]), c(y[1], 5), d=d, d.head=d.head, lwd=2);
a4 = arrowSimple(c(x[1], x[1]+2), c(y[1]+1, 5), d=d, d.head=d.head, lwd=2);
# Head
h1 = a1$Head[[1]]
h2 = a2$Head[[1]]
h3 = a3$Head[[1]]
h4 = a4$Head[[1]]
# - visual aids:
lines(h1$x[c(1,3)], h1$y[c(1,3)], col="green")
lines(h2$x[c(1,3)], h2$y[c(1,3)], col="green")
lines(h3$x[c(1,3)], h3$y[c(1,3)], col="green")
lines(h4$x[c(1,3)], h4$y[c(1,3)], col="green")
stopifnot(round(Dsquare(h1, h1$x[2], h1$y[2]) - 2*d^2 - sum(d.head^2), 8) == 0)
stopifnot(round(Dsquare(h2, h2$x[2], h2$y[2]) - 2*d^2 - sum(d.head^2), 8) == 0)
stopifnot(round(Dsquare(h3, h3$x[2], h3$y[2]) - 2*d^2 - sum(d.head^2), 8) == 0)
stopifnot(round(Dsquare(h4, h4$x[2], h4$y[2]) - 2*d^2 - sum(d.head^2), 8) == 0)

###### Test 3 ######
plot.base(ylim = c(0,100))
x = c(0, 6); y = c(1, 80);
d = -2; d.head = c(-3, 3);
scale = (100/12)*aspect_ratio_max  #1.4;
a1 = arrowSimple(x, y, d=d, d.head=d.head, lwd=2, scale=scale);
a2 = arrowSimple(c(x[1], 5), c(y[1], y[1]), d=d, d.head=d.head, lwd=2, scale=scale);
a3 = arrowSimple(c(x[1], x[1]), c(y[1], 50), d=d, d.head=d.head, lwd=2, scale=scale);
a4 = arrowSimple(c(x[1], x[1]+2), c(y[1]+20, 50), d=d, d.head=d.head, lwd=2, scale=scale);
# Head
h1 = a1$Head[[1]]
h2 = a2$Head[[1]]
h3 = a3$Head[[1]]
h4 = a4$Head[[1]]
# - visual aids:
lines(h1$x[c(1,3)], h1$y[c(1,3)], col="green")
lines(h2$x[c(1,3)], h2$y[c(1,3)], col="green")
lines(h3$x[c(1,3)], h3$y[c(1,3)], col="green")
lines(h4$x[c(1,3)], h4$y[c(1,3)], col="green")

##### Double lined ArrowHead #####

###### default join: join through #####
# - comparison between different dV (d.head argument);

###### Test 1 ######
x = c(0, 6); y = c(1, 6);
d = -1; d2 = -1.5;
plot.base()
a1 = arrowDouble(x, y, d=d, lwd=2);
a2 = arrowDouble(c(x[1], 5), c(y[1], y[1]), d=d, lwd=2);
a3 = arrowDouble(c(x[1], x[1]), c(y[1], 5), d=d, lwd=2);
# Head
h1 = a1$Head[[1]]
h2 = a1$Head[[2]]

h3 = a2$Head[[1]]
h4 = a2$Head[[2]]

h5 = a3$Head[[1]]
h6 = a3$Head[[2]]
# - visual aids:
lines(h1$x[c(1,3)], h1$y[c(1,3)], col="green")
lines(h2$x[c(1,3)], h2$y[c(1,3)], col="green")

lines(h3$x[c(1,3)], h3$y[c(1,3)], col="green")
lines(h4$x[c(1,3)], h4$y[c(1,3)], col="green")

lines(h5$x[c(1,3)], h5$y[c(1,3)], col="green")
lines(h6$x[c(1,3)], h6$y[c(1,3)], col="green")
# Total length = (d^2 + dV[1]^2) + (d^2 + dV[2]^2)
stopifnot(round(Dsquare(h1, h1$x[2], h1$y[2]) - 2*d^2 - 2*d^2, 8) == 0)
stopifnot(round(Dsquare(h2, h2$x[2], h2$y[2]) - 2*d^2 - 2*d^2, 8) == 0)
stopifnot(round(Dsquare(h3, h3$x[2], h3$y[2]) - 2*d^2 - 2*d^2, 8) == 0)


###### Test 2 ######
###### de modificat la tot double ######
x = c(0, 6); y = c(1, 6) + 1;
d=-0.5; dV = c(-1.5,1.5);
d.head = -1.5;
plot.base()
a1 = arrowDouble(x, y, d=d, d.head=d.head, dV=dV, lwd=2);
a2 = arrowDouble(c(x[1], 8), c(y[1], y[1]), d=d, dV=dV, d.head=d.head, lwd=2);
a3 = arrowDouble(c(x[1], x[1]), c(y[1], 8), d=d, dV=dV, d.head=d.head, lwd=2);
# Head
h1 = a1$Head[[1]]
h2 = a1$Head[[2]]

h3 = a2$Head[[1]]
h4 = a2$Head[[2]]

h5 = a3$Head[[1]]
h6 = a3$Head[[2]]
# - visual aids:
lines(h1$x[c(1,3)], h1$y[c(1,3)], col="green")
lines(h2$x[c(1,3)], h2$y[c(1,3)], col="green")

lines(h3$x[c(1,3)], h3$y[c(1,3)], col="green")
lines(h4$x[c(1,3)], h4$y[c(1,3)], col="green")

lines(h5$x[c(1,3)], h5$y[c(1,3)], col="green")
lines(h6$x[c(1,3)], h6$y[c(1,3)], col="green")

stopifnot(round(Dsquare(h1, h1$x[2], h1$y[2]) - 2*d.head^2 - sum(dV^2), 8) == 0)
stopifnot(round(Dsquare(h2, h2$x[2], h2$y[2]) - 2*d.head^2 - sum(dV^2), 8) == 0)
stopifnot(round(Dsquare(h3, h3$x[2], h3$y[2]) - 2*d.head^2 - sum(dV^2), 8) == 0)

###### Test 3 ######
plot.base(ylim = c(0,100))
x = c(0, 6); y = c(1, 80);
d = -2; d.head = -3;
scale = (100/12)*aspect_ratio_max  #1.4;
a1 = arrowDouble(x, y, d=d, d.head=d.head, lwd=2, scale=scale);
a2 = arrowDouble(c(x[1], 5), c(y[1], y[1]), d=d, d.head=d.head, lwd=2, scale=scale);
a3 = arrowDouble(c(x[1], x[1]), c(y[1], 50), d=d, d.head=d.head, lwd=2, scale=scale);
# Head
h1 = a1$Head[[1]]
h2 = a1$Head[[2]]

h3 = a2$Head[[1]]
h4 = a2$Head[[2]]

h5 = a3$Head[[1]]
h6 = a3$Head[[2]]
# - visual aids:
lines(h1$x[c(1,3)], h1$y[c(1,3)], col="green")
lines(h2$x[c(1,3)], h2$y[c(1,3)], col="green")

lines(h3$x[c(1,3)], h3$y[c(1,3)], col="green")
lines(h4$x[c(1,3)], h4$y[c(1,3)], col="green")

lines(h5$x[c(1,3)], h5$y[c(1,3)], col="green")
lines(h6$x[c(1,3)], h6$y[c(1,3)], col="green")

###### join with innermost ">" #####
###### Test 1 ######
x = c(0, 6); y = c(1, 6);
d = -1; d2 = -1.5;
plot.base()
a1 = arrowDouble(x, y, d=d, lwd=2, join=1);
a2 = arrowDouble(c(x[1], 5), c(y[1], y[1]), d=d, lwd=2, join=1);
a3 = arrowDouble(c(x[1], x[1]), c(y[1], 5), d=d, lwd=2, join=1);
# Head
h1 = a1$Head[[1]]
h2 = a1$Head[[2]]

h3 = a2$Head[[1]]
h4 = a2$Head[[2]]

h5 = a3$Head[[1]]
h6 = a3$Head[[2]]
# - visual aids:
lines(h1$x[c(1,3)], h1$y[c(1,3)], col="green")
lines(h2$x[c(1,3)], h2$y[c(1,3)], col="green")

lines(h3$x[c(1,3)], h3$y[c(1,3)], col="green")
lines(h4$x[c(1,3)], h4$y[c(1,3)], col="green")

lines(h5$x[c(1,3)], h5$y[c(1,3)], col="green")
lines(h6$x[c(1,3)], h6$y[c(1,3)], col="green")
# Total length = (d^2 + dV[1]^2) + (d^2 + dV[2]^2)
stopifnot(round(Dsquare(h1, h1$x[2], h1$y[2]) - 2*d^2 - 2*d^2, 8) == 0)
stopifnot(round(Dsquare(h2, h2$x[2], h2$y[2]) - 2*d^2 - 2*d^2, 8) == 0)
stopifnot(round(Dsquare(h3, h3$x[2], h3$y[2]) - 2*d^2 - 2*d^2, 8) == 0)


###### Test 2 ######
plot.base()
x = c(0, 6); y = c(1, 6) + 1;
d=-1.5; d.head = -1.5; dV = c(-1.5,1.5);
a1 = arrowDouble(x, y, d=d, d.head=d.head, dV=dV, lwd=2, join=1);
a2 = arrowDouble(c(x[1], 8), c(y[1], y[1]), d=d, d.head=d.head, lwd=2, join=1);
a3 = arrowDouble(c(x[1], x[1]), c(y[1], 8), d=d, d.head=d.head, lwd=2, join=1);
# Head
h1 = a1$Head[[1]]
h2 = a1$Head[[2]]

h3 = a2$Head[[1]]
h4 = a2$Head[[2]]

h5 = a3$Head[[1]]
h6 = a3$Head[[2]]
# - visual aids:
lines(h1$x[c(1,3)], h1$y[c(1,3)], col="green")
lines(h2$x[c(1,3)], h2$y[c(1,3)], col="green")

lines(h3$x[c(1,3)], h3$y[c(1,3)], col="green")
lines(h4$x[c(1,3)], h4$y[c(1,3)], col="green")

lines(h5$x[c(1,3)], h5$y[c(1,3)], col="green")
lines(h6$x[c(1,3)], h6$y[c(1,3)], col="green")

stopifnot(round(Dsquare(h1, h1$x[2], h1$y[2]) - 2*d.head^2 - sum(dV^2), 8) == 0)
stopifnot(round(Dsquare(h2, h2$x[2], h2$y[2]) - 2*d.head^2 - sum(dV^2), 8) == 0)
stopifnot(round(Dsquare(h3, h3$x[2], h3$y[2]) - 2*d.head^2 - sum(dV^2), 8) == 0)

###### Test 3 ######
plot.base(ylim = c(0,100))
x = c(0, 6); y = c(1, 80);
d = -2; d.head = -3;
scale = (100/12)*aspect_ratio_max  #1.4;
a1 = arrowDouble(x, y, d=d, d.head=d.head, lwd=2, scale=scale, join=1);
a2 = arrowDouble(c(x[1], 5), c(y[1], y[1]), d=d, d.head=d.head, lwd=2, scale=scale, join=1);
a3 = arrowDouble(c(x[1], x[1]), c(y[1], 50), d=d, d.head=d.head, lwd=2, scale=scale, join=1);
# Head
h1 = a1$Head[[1]]
h2 = a1$Head[[2]]

h3 = a2$Head[[1]]
h4 = a2$Head[[2]]

h5 = a3$Head[[1]]
h6 = a3$Head[[2]]
# - visual aids:
lines(h1$x[c(1,3)], h1$y[c(1,3)], col="green")
lines(h2$x[c(1,3)], h2$y[c(1,3)], col="green")

lines(h3$x[c(1,3)], h3$y[c(1,3)], col="green")
lines(h4$x[c(1,3)], h4$y[c(1,3)], col="green")

lines(h5$x[c(1,3)], h5$y[c(1,3)], col="green")
lines(h6$x[c(1,3)], h6$y[c(1,3)], col="green")

##### Inverted Head #####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowInverted(x, y, d=1, lwd=2);
arrowInverted(c(x[1], 5), c(y[1], y[1]), d=1, lwd=2);
arrowInverted(c(x[1], x[1]), c(y[1], 5), d=1, lwd=2);

### Inhomogeneous Axes
plot.base(ylim = c(0, 100))
scale = - (100/12)*aspect_ratio_max;
d = 3;
x = c(0, 6); y = c(1, 60);
arrowInverted(x, y, d=d, lwd=2, scale=scale);
arrowInverted(c(x[1], 5), c(y[1], y[1]), d=d, lwd=2, scale=scale);
arrowInverted(c(x[1], x[1]), c(y[1], 50), d=d, lwd=2, scale=scale);

##### Diamond ArrowHead #####

# default join
plot.base()
x = c(0, 6); y = c(1, 6);
arrowDiamond(x, y, d=-1, lwd=2);
arrowDiamond(c(x[1], 5), c(y[1], y[1]), d=-1, lwd=2);
arrowDiamond(c(x[1], x[1]), c(y[1], 5), d=-1, lwd=2);

# join through
plot.base()
x = c(0, 6); y = c(1, 6);
arrowDiamond(x, y, d=-1, lwd=2, join=2);
arrowDiamond(c(x[1], 5), c(y[1], y[1]), d=-1, lwd=2, join=2);
arrowDiamond(c(x[1], x[1]), c(y[1], 5), d=-1, lwd=2, join=2);

##### T Shape ArrowHead #####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowT(x, y, d=-1, lwd=2);
arrowT(c(x[1], 5), c(y[1], y[1]), d=-1, lwd=2);
arrowT(c(x[1], x[1]), c(y[1], 5), d=-1, lwd=2);

##### Measurement ArrowHead #####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowMeasure(x, y, d=-1, lwd=2);
arrowMeasure(c(x[1], 5), c(y[1], y[1]), d=-1, lwd=2);
arrowMeasure(c(x[1], x[1]), c(y[1], 5), d=-1, lwd=2);

### one line
dT = c(-1); dV = c(-1,1) / 2;
plot.base()
x = c(0, 6); y = c(1, 6);
arrowMeasure(x, y, d=-1, d.head=dV, dT=dT, lwd=2);
arrowMeasure(c(x[1], 5), c(y[1], y[1]), d=-1, d.head=dV, dT=dT, lwd=2);
arrowMeasure(c(x[1], x[1]), c(y[1], 5), d=-1, d.head=dV, dT=dT, lwd=2);

##### X Shape ArrowHead #####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowX(x, y, d=-1, lwd=2);
arrowX(c(x[1], 5), c(y[1], y[1]), d=-1, lwd=2);
arrowX(c(x[1], x[1]), c(y[1], 5), d=-1, lwd=2);

##### Square Shape ArrowHead #####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowSquare(x, y, d=-1, lwd=2);
arrowSquare(c(x[1], 5), c(y[1], y[1]), d=-1, lwd=2);
arrowSquare(c(x[1], x[1]), c(y[1], 5), d=-1, lwd=2);

##### Multiple-Lined ArrowHead #####

# default join:
n = 5; d = 1;
plot.base()
x = c(0, 6); y = c(1, 6);
arrowN(x, y, n=n, d=d, lwd=2);
arrowN(c(x[1], 8), c(y[1], y[1]), n=n, d=d, lwd=2);
arrowN(c(x[1], x[1]), c(y[1], 8), n=n, d=d, lwd=2);

# explicit join:
n = 5; d = 0.5;
join = n;
plot.base()
x = c(0, 6); y = c(1, 6);
arrowN(x, y, n=n, d=d, lwd=2, join=join);
arrowN(c(x[1], 8), c(y[1], y[1]), n=n, d=d, lwd=2, join=join);
arrowN(c(x[1], x[1]), c(y[1], 8), n=n, d=d, lwd=2, join=join);

##### Double Lined Inverted ArrowHead #####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowDoubleInverted(x, y, d=-1, lwd=2);
arrowDoubleInverted(c(x[1], 5), c(y[1], y[1]), d=-1, lwd=2);
arrowDoubleInverted(c(x[1], x[1]), c(y[1], 5), d=-1, lwd=2);

##### Circle ArrowHead #####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowCircle(x, y, r=0.5, lwd=2);
arrowCircle(c(x[1], 5), c(y[1], y[1]), r=0.5, lwd=2);
arrowCircle(c(x[1], x[1]), c(y[1], 5), r=0.5, lwd=2);

##### Solid Square ArrowHead #####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowSolidSquare(x, y, d=-1, lwd=2);
arrowSolidSquare(c(x[1], 5), c(y[1], y[1]), d=-1, lwd=2);
arrowSolidSquare(c(x[1], x[1]), c(y[1], 5), d=-1, lwd=2);

##### Triangle  ArrowHead ####

### Test 1
x = c(0, 6); y = c(1, 6);
d = -1;
plot.base()
a1 = arrowTriangle(x, y, d=d, lwd=2);
a2 = arrowTriangle(c(x[1], 5), c(y[1], y[1]), d=d, lwd=2);
a3 = arrowTriangle(c(x[1], x[1]), c(y[1], 5), d=d, lwd=2);
# Head
h1 = a1$Head[[1]]
h2 = a2$Head[[1]]
h3 = a3$Head[[1]]
# 4 edges: d * c(sqrt(2), sqrt(2), 0, 2);
# => edge 1 is counted twice; edge 4 is not counted;
stopifnot(round(Dsquare(h1, h1$x[2], h1$y[2]) - 6*d^2, 8) == 0)
stopifnot(round(Dsquare(h2, h2$x[2], h2$y[2]) - 6*d^2, 8) == 0)
stopifnot(round(Dsquare(h3, h3$x[2], h3$y[2]) - 6*d^2, 8) == 0)

#### Simple  ArrowHead ####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowSimple(x, y, d=-1, lwd=2);

#### Double lined ArrowHead ####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowDouble(x, y, d=0.5, lwd=2);

#### T Shape ArrowHead ####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowT(x, y, d=2, lwd=2);

#### X Shape ArrowHead ####
plot.base()
x = c(0, 6); y = c(1, 8);
arrowX(x, y, d=1.5, lwd=2)

### X: Different lengths
plot.base()
x = c(0, 6); y = c(1, 8);
arrowX(x, y, d=c(0.75, 2.0), d.head=c(-1,1), lwd=2)
abline(v=x[2], col="green");
points(x[2], y[2], col="red")

#### Square Shape ArrowHead ####
plot.base()
x = c(0, 6); y = c(1, 8);
arrowSquare(x, y, d=1.5, lwd=2)

#### Multiple-lined ArrowHead ####
# TODO: Junction
plot.base()
x = c(0, 6); y = c(1, 6);
arrowN(x, y, n = 5, d=0.5, lwd=2)

#### Double Lined Inverted ArrowHead ####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowDoubleInverted(x, y, lwd=2);
abline(v=x[2], col="green");
points(x[2], y[2], col="red")


#### Double lined ArrowHead ####
# TODO: Junction
plot.base()
x = c(0, 6); y = c(1, 6);
arrowDouble(x, y, d=0.5, lwd=2);
abline(v=x[2], col="green");
points(x[2], y[2], col="black")

#### Circle Arrowhead ####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowCircle(x, y, r=0.5, lwd=2);
abline(v=x[2], col="green");
points(x[2], y[2], col="red")

#### Solid Square ArrowHead ####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowSolidSquare(x, y, d=-2, lwd=2);
abline(v=x[2], col="green");
points(x[2], y[2], col="red")

#### Triangle  ArrowHead ###

### Test 1
x = c(0, 6); y = c(1, 6);
d = -1;
plot.base()
a1 = arrowTriangle(x, y, d=d, lwd=2);
a2 = arrowTriangle(c(x[1], 5), c(y[1], y[1]), d=d, lwd=2);
a3 = arrowTriangle(c(x[1], x[1]), c(y[1], 5), d=d, lwd=2);
# Head
h1 = a1$Head[[1]]
h2 = a2$Head[[1]]
h3 = a3$Head[[1]]
# 4 edges: d * c(sqrt(2), sqrt(2), 0, 2);
# => edge 1 is counted twice; edge 4 is not counted;
stopifnot(round(Dsquare(h1, h1$x[2], h1$y[2]) - 6*d^2, 8) == 0)
stopifnot(round(Dsquare(h2, h2$x[2], h2$y[2]) - 6*d^2, 8) == 0)
stopifnot(round(Dsquare(h3, h3$x[2], h3$y[2]) - 6*d^2, 8) == 0)

