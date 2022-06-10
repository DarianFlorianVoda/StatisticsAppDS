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



install.packages("testthat")
install.packages("Rtools")
