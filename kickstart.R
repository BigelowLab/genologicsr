# kickstart.R
# used during development

library(XML)
library(httr)

configfile <- "~/.clarityrc"

path <- "/Users/Shared/code/R/others/gls"
sfiles <- "Lims.R"
for (sf in sfiles) source(file.path(path, sf))

lims <- Lims(configfile)
