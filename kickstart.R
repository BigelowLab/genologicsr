# kickstart.R
# used during development

library(XML)
library(httr)

configfile <- "~/.clarityrc"

path <- "/Users/Shared/code/R/others/genologicsR/R"
sfiles <- list.files(path, full.names = TRUE)
for (sf in sfiles) { source(sf)}

lims <- Lims(configfile)


node <- lims$get_byLimsid("27-1", resource = "containers")[[1]]
C <- Container$new(node, lims)


nodes <- lims$get_containers(name = 'BEN_1')


# PROCESS
node <- lims$get_byLimsid('24-798', resource = "processes")[[1]]
P <- Process$new(node, lims)
P$PUT()


# SAMPLE
node<- lims$get_byLimsid("FER101A1", resource = 'samples')[[1]]
S <- Node$new(node = node, lims = lims)

# change something in S and then S$PUT()
