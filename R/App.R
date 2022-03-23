#'TODO: source correct the directory path
#' Set your own path to your working directory
path = "C:/Users/daria/OneDrive/Desktop/Licenta/Diagram-Generator/Diagram.Generator/R"
setwd(path)

source("Graphic.Helper.R");
source("Graphic.Arrows.R");
source("Graphic.Arrows.Heads.R");
source("Graphic.Objects.R");
source("Graphic.Chemistry.R");

#### Tests ####
source("Graphic.Arrows.Tests.R")
source("Graphic.Helper.Tests.R")
source("Graphic.Objects.Tests.R")
source("Graphic.Chemistry.Tests.R")

#### Examples #####
source("Examples.R")
