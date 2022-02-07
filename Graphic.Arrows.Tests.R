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


### Tests & Examples

#####################
### Test Helper Functions

### Example 1:

# Base-Line
# TODO: various origins, e.g.(x0=0, y0=0) vs non-zero;
# Note: x = (x0, x1); y = (y0, y1);

### Arrows:

### Ex 1:
plot.base()
x = c(0,10); y = c(1, 5);
arrowDH(x, y, lwd=2, asD=TRUE);

### Ex 2:
x = c(0, 6); y = c(1, 6);
arrowInvH(x, y, lwd=2);

### Ex 3:
arrowDH(x, y*2 - 2, d = 0.3, lwd=2, col="blue", asD=TRUE);


#############
### Other ###

### Test: Banded lines
plot.base()
lineBanded(c(0,5), c(1, 8), lwd=2.5)
lineBanded(c(1,7), c(0, 5), lwd=2.5)
lineBanded(c(0,6), c(6, 0), lwd=2.5, col="green")