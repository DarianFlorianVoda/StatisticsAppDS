aspect_ratio = 1.4
aspect_ratio_max = 2.0
# aspect_ratio_max = 1.0

#### Helper Functions ####

Dsquare = function(xy, x0, y0) {
  sum((xy$x - x0)^2, (xy$y - y0)^2)
}

testArrow = function(h, d, dV=c(-d, d)){
  stopifnot(round(Dsquare(h, h$x[2], h$y[2]) - 2*d^2 - sum(dV^2), 8) == 0)
}

linesAid = function(..., id=c(1,3)) {
  h = list(...);
  lapply(h, linesAid1, id=id);
  invisible();
}
linesAid1 = function(h, id = c(1,3), col="green") {
  lines(h$x[id], h$y[id], col=col)
}
