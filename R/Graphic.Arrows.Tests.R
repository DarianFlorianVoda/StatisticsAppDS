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


#### Tests ####

#### Horizontal Tests ####

##### Simple  ArrowHead ####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowSimpleH(x, y, d=-1, lwd=2);
arrowSimpleH(c(x[1], 5), c(y[1], y[1]), d=-1, lwd=2);
arrowSimpleH(c(x[1], x[1]), c(y[1], 5), d=-1, lwd=2);
arrowSimpleH(x, y+2, d=-1.5, d.head=c(-0.5, 0.5), lwd=2);

##### Inverted Head #####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowInvH(x, y, d=-1, lwd=2);
arrowInvH(c(x[1], 5), c(y[1], y[1]), d=-1, lwd=2);
arrowInvH(c(x[1], x[1]), c(y[1], 5), d=-1, lwd=2);

##### Diamond ArrowHead #####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowDiamondH(x, y, d=-1, lwd=2);
arrowDiamondH(c(x[1], 5), c(y[1], y[1]), d=-1, lwd=2);
arrowDiamondH(c(x[1], x[1]), c(y[1], 5), d=-1, lwd=2);

##### Double lined ArrowHead #####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowDH(x, y, d=-1, lwd=2);
arrowDH(c(x[1], 5), c(y[1], y[1]), d=-1, lwd=2);
arrowDH(c(x[1], x[1]), c(y[1], 5), d=-1, lwd=2);

##### T Shape ArrowHead #####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowTH(x, y, d=-1, lwd=2);
arrowTH(c(x[1], 5), c(y[1], y[1]), d=-1, lwd=2);
arrowTH(c(x[1], x[1]), c(y[1], 5), d=-1, lwd=2);

##### X Shape ArrowHead #####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowXH(x, y, d=-1, lwd=2);
arrowXH(c(x[1], 5), c(y[1], y[1]), d=-1, lwd=2);
arrowXH(c(x[1], x[1]), c(y[1], 5), d=-1, lwd=2);

##### Square Shape ArrowHead #####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowSQ(x, y, d=-1, lwd=2);
arrowSQ(c(x[1], 5), c(y[1], y[1]), d=-1, lwd=2);
arrowSQ(c(x[1], x[1]), c(y[1], 5), d=-1, lwd=2);

##### Multiple-lined ArrowHead #####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowMultiH(x, y, d=-1, n=5, lwd=2);
arrowMultiH(c(x[1], 5), c(y[1], y[1]), n=5, d=-1, lwd=2);
arrowMultiH(c(x[1], x[1]), c(y[1], 5), n=5, d=-1, lwd=2);

##### Double Lined Inverted ArrowHead #####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowInv2H(x, y, d=-1, lwd=2);
arrowInv2H(c(x[1], 5), c(y[1], y[1]), d=-1, lwd=2);
arrowInv2H(c(x[1], x[1]), c(y[1], 5), d=-1, lwd=2);

##### Circle ArrowHead #####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowCircle(x, y, r=0.5, lwd=2);
arrowCircle(c(x[1], 5), c(y[1], y[1]), r=0.5, lwd=2);
arrowCircle(c(x[1], x[1]), c(y[1], 5), r=0.5, lwd=2);

##### Solid Square ArrowHead #####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowSolidSQ(x, y, d=-1, lwd=2);
arrowSolidSQ(c(x[1], 5), c(y[1], y[1]), d=-1, lwd=2);
arrowSolidSQ(c(x[1], x[1]), c(y[1], 5), d=-1, lwd=2);

#### Inverted ArrowHead ####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowInvH(x, y, lwd=2);

#### Diamond ArrowHead ####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowDiamondH(x, y, d= 0.5, lwd=2);

#### Simple  ArrowHead ####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowSimpleH(x, y, d=-1, lwd=2);

#### Double lined ArrowHead ####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowDH(x, y, d=0.5, lwd=2);

#### T Shape ArrowHead ####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowTH(x, y, d=2, lwd=2);

#### X Shape ArrowHead ####
plot.base()
x = c(0, 6); y = c(1, 8);
arrowXH(x, y, d=1.5, lwd=2)

### X: Different lengths
plot.base()
x = c(0, 6); y = c(1, 8);
arrowXH(x, y, d=c(0.75, 2.0), d.head=c(-1,1), lwd=2)
abline(v=x[2], col="green");
points(x[2], y[2], col="red")

#### Square Shape ArrowHead ####
plot.base()
x = c(0, 6); y = c(1, 8);
arrowSQ(x, y, d=1.5, lwd=2)

#### Multiple-lined ArrowHead ####
# TODO: Junction
plot.base()
x = c(0, 6); y = c(1, 6);
arrowMultiH(x, y, n = 5, d=0.5, lwd=2)

#### Double Lined Inverted ArrowHead ####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowInv2H(x, y, lwd=2);
abline(v=x[2], col="green");
points(x[2], y[2], col="red")


#### Double lined ArrowHead ####
# TODO: Junction
plot.base()
x = c(0, 6); y = c(1, 6);
arrowDH(x, y, d=0.5, lwd=2);
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
arrowSolidSQ(x, y, d=-2, lwd=2);
abline(v=x[2], col="green");
points(x[2], y[2], col="red")
