library('devtools')

setwd("~/GitHub/Scotty")

## Build
devtools::document()
devtools::check()
devtools::build()
devtools::test()
pkgdown::build_site()

