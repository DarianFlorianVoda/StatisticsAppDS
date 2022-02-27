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

#### Simple Arrows ####

#### Inverted Head ####:
plot.base()
x = c(0, 6); y = c(1, 6);
arrowInvH(x, y, lwd=2);

#### Diamond ArrowHead ####:
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

#### Square Shape ArrowHead ####
plot.base()
x = c(0, 6); y = c(1, 8);
arrowSQ(x, y, d=1.5, lwd=2)

#### Double lined ArrowHead ####
plot.base()
x = c(0, 6); y = c(1, 6);
arrowMultiH(x, y, d=0.5, lwd=2, n=5)
