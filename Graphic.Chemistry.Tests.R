#### Poly-cyclic chemical molecules ####

#### Cholesterol backbone ####
plot.base()
p = parseCycles("6|6\\6|5")
lines(p)
lines(ligand(p, c(1,3), c(1,1), c(Inf, Inf), c(T,T)))

##### [1] Pentacyclic Triterpene backbone ####
plot.base()
lines(parseCycles("6|6\\6|6\\6"))

#### [2] Triterpenoids ####
#this does not work yet, but can be added as a test
# TODO
plot.base()
lines(parseCycles("6|6\\6|6\\5"))

#### [3] Molecules from vegetables; ####
##### Psoralens & Pabulenol quasi-backbone #####
# - vegetables: parsnip (pastarnac) & parsley (patrunjel);
plot.base()
lines(parseCycles("6|6|5"))

### Etoposide backbone
plot.base()
lines(parseCycles("<5|6|6|5"))

#### Spiro-Derivatives ####
plot.base()
lines.arrow(list(parseCycles("6|6|5<5"), list()))

plot.base()
lines.arrow(list(parseCycles("6|6|5<7"), list()))

#### Other ####
plot.base()
lines.arrow(list(parseCycles("5-5"), list()))

#### Sesquifulvalene backbone ####
plot.base()
lines.arrow(list(parseCycles("7=5"), list()))

