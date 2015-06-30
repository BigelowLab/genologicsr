# kickstart.R
# used during development

library(XML)
library(httr)

# source in the correct order - because of inheritance issue
path <- "/Users/Shared/code/R/others/genologicsR/R"
sfiles <- c("misc.R", "Lims.R", "Node.R","Container.R", 
   "File.R", "Process.R", "Sample.R", "Udf.R")
for (sf in sfiles) source(file.path(path, sf))

lims <- Lims(configfile = "~/.clarityrc")

# CONTAINER
node <- lims$get_byLimsid("27-1", resource = "containers")[[1]]
C <- Container$new(node, lims)
# or equivalently
C <- lims$get_byLimsid("27-1", resource = "containers", asNode = TRUE)[[1]]

# get containers by name
CC <- lims$get_containers(name = c("Liz_test_1", "Liz_test_2") )



# PROCESS
node <- lims$get_byLimsid('24-798', resource = "processes")[[1]]
P <- Process$new(node, lims)
# or equivalently
P <- lims$get_byLimsid('24-798', resource = "processes", asNode = TRUE)[[1]]
# make some changes to P here and then PUT to the database
P$PUT()


# SAMPLE
node<- lims$get_byLimsid("FER101A1", resource = 'samples')[[1]]
S <- Sample$new(node = node, lims = lims)
# or equivalently
S <- lims$get_byLimsid("FER101A1", resource = 'samples', asNode = TRUE)[[1]]

# get samples by projectlimsid (or name, or projectname) - takes a while as there 
# are 96 of these
S <- lims$get_samples(projectlimsid = "FER101")
