#### Poly-cyclic chemical molecules ####

### Cholesterol backbone
plot.base()
lines(parseCycles("6|6\\6|5"))

### Triterpene backbone
plot.base()
lines(parseCycles("6|6\\6|6\\6"))

### Spiro compounds
plot.base()
lines(parseCycles("6|6|5<5"))

plot.base()
lines(parseCycles("6|6|5<7"))

plot.base()
lines(parseCycles("6|6|6|5<3"))

### Other
plot.base()
lines(parseCycles("5-5"))

### Sesquifulvalene backbone
plot.base()
lines(parseCycles("7=5"))
