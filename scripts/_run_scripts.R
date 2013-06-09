source("_setup.R")

setwd(gv$scripts)
spin("scenarios.R")

unlink("figure", recursive = TRUE)
unlink("*.Rmd")
unlink("*.md")

setwd(gv$root)
