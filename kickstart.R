# kickstart.R
# used during development

library(XML)
library(httr)

configfile <- "~/.clarityrc"

path <- "/Users/Shared/code/R/others/glsR/R"
sfiles <- c("misc.R", "Lims.R", "Node.R", "Process.R")
for (sf in sfiles) source(file.path(path, sf))

lims <- Lims(configfile)

#dummy <- Node$new()

# PROCESS
node <- lims$getByLimsid('24-798', resource = "processes")[[1]]
P <- Process$new(node, lims)
P$PUT()


# SAMPLE
node<- lims$getByLimsid("FER101A1", resource = 'samples')[[1]]
S <- Node$new(node = node, lims = lims)

# change something in S and then S$PUT()
