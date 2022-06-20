library("devtools")
getwd()
setwd("C:/Users/daria/OneDrive/Desktop/Licenta/Diagram-Generator/Diagram.Generator")
devtools::document()
devtools::build_vignettes("introduction")
devtools::install()
library("Diagram.Generator")
devtools::install_github(repo='DarianFlorianVoda/Diagram.Generator')
usethis::gh_token_help()
usethis::git_sitrep()

usethis::edit_r_environ()
gitcreds::gitcreds_set('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')


devtools::load_all()
devtools::test()
devtools::check()

# Useful commands:
devtools::use_r() # creates an .R file in R folder
devtools::use_test() # creates an test file in tests folder
devtools::document() # conversion

devtools::update_packages("devtools")

## Now do Code > Insert roxygen skeleton

devtools::use_readme_md()
build_readme() # it takes care to render with the most current version of your package

devtools::dev_sitrep()

install.packages("available")
remove.packages("rlang")
devtools::install_github("tidyverse/tidyverse")
available::available("BioShapes")

citation() # cite R (default) or some package

install.packages("testthat")
install.packages("Rtools")
