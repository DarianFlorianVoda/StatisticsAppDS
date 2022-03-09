#### poly-cyclic chemical molecules ####
# numbers = dimension of cycle;
# separators: type of junction between cycles;
parseCycles = function(x, r=1, d2=0.0625) {
  reg = "(?<=[0-9])(?=[^0-9])|(?<=[^0-9])(?=[0-9])";
  cyc = strsplit(x, reg, perl=TRUE);
  cyc = cyc[[1]];
  #
  l = list();
  x0 = 0; y0 = 0; r0 = r;
  r  = 0; d  = 0;
  phi0 = 0; phi = 0; s = "";
  nL = as.numeric(cyc);
  for(id in seq(length(cyc))) {
    n = nL[id];
    if( ! is.na(n)) {
      halpha = pi/n;
      r = r0 / 2 / sin(halpha);
      d = r * cos(halpha);
      # Correction to rotation:
      dp = if(phi0 == 0) d else (d + r + d / cos(phi + phi0));
      phi = if(n %% 2 == 1) 0 else halpha;
      lPp = pointsCircle(n, r=r, center=c(x0 + dp, y0), phi = phi + phi0);
      lPp$x = c(lPp$x, lPp$x[1]);
      lPp$y = c(lPp$y, lPp$y[1]);
      # Reset phi0
      if(phi0 != 0 && round(phi0 - pi, 8) == 0) {
        phi0 = 0;
      }
      l = c(l, list(lPp));
      next;
    }
    s = cyc[id];
    if(s == "|") {
      # TODO: "5<5|4"
      x0 = x0 + d + (if(nL[id - 1] %% 2 == 1) r else d);
      next;
    }
    if(s == "<") {
      x0 = x0 + d + r;
      phi0 = phi0 + pi;
      next;
    }
    if(s == "-" || s == "=") {
      # TODO: if(phi != 0)
      x0 = x0 + d + r;
      x1 = x0 + r0;
      l1 = list(x = c(x0, x1), y = c(y0, y0));
      if(s == "=") {
        l2 = shiftLine(l1$x, l1$y, d = c(-d2, d2));
        l1 = split(l2[, 1:2], l2$id);
      } else l1 = list(l1);
      l = c(l, l1);
      x0 = x1;
      phi0 = phi0 + pi;
      next;
    }
    if(s == "\\") {
      # TODO: now only for n = 6;
      x0 = x0 + d;
      y0 = y0 + r + r0/2;
      next;
    }
  }
  class(l) = c("chemistry", class(l));
  return(l)
}
