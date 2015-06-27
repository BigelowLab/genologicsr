# kickstart.R
# used during development

library(XML)
library(httr)

configfile <- "~/.clarityrc"

path <- "/Users/Shared/code/R/others/gls"
sfiles <- c("misc.R", "Lims.R")
for (sf in sfiles) source(file.path(path, sf))

X <- Lims(configfile)

filelimsid <- '92-1554'

x <- X$getByLimsid(filelimsid)

ok <- X$DELETE(x[[1]])