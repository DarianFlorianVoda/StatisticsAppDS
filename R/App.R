#'TODO: source correct the directory path
#' Set your own path to your working directory
path = "C:/Users/daria/OneDrive/Desktop/Licenta/Diagram.Generator/R"
setwd(path)

source("Graphic.Helper.R");
source("Graphic.Arrows.R");
source("Graphic.Arrows.Heads.R");
source("Graphic.Objects.R");
source("Graphic.Chemistry.R");

#### Tests ####
source("Tests/Graphic.Arrows.Tests.R")
source("Tests/Graphic.Helper.Tests.R")
source("Tests/Graphic.Objects.Tests.R")
source("Tests/Graphic.Chemistry.Tests.R")

#### Examples #####
source("Examples.R")
