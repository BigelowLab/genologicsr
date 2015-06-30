#### History

We created our own API wrapper in R to access the GLS API v1 using [curl](http://curl.haxx.se/) and [libxml2](http://xmlsoft.org/) via the R packages [RCurl](http://cran.r-project.org/web/packages/RCurl/index.html) and [XML](http://cran.r-project.org/web/packages/XML/index.html).  This R wrapper is privately distributed in the glsAPI package. The maintainer of the RCurl and XML packages is very busy, and while the packages are stable the update/fix cycle can be slow.  Of the two, XML has wide support from others and so it seems less orphan-like.

#### New direction

The arrival of [GLS API v2](http://genologics.com/developer) and Clarity LIMS it is appropriate to take stock of the resources available to us and make improvements.  To this end we have started to develop the genologicsR package model after the python [genologics](https://github.com/SciLifeLab/genologics) module.  We still use the XML package, but in lieue of RCurl we use the [httr](http://cran.r-project.org/web/packages/httr/index.html) package which provides a uniform interface to [RESTful](https://en.wikipedia.org/wiki/Representational_state_transfer) http transactions.  httr depends upon the [curl](http://cran.r-project.org/web/packages/curl/index.html) package which appears to be vigourously maintained.  httr and curl are maintained by people active with the RStudio project so the chances for longevity look bright.

#### genologicsR

Like the python [genologics](https://github.com/SciLifeLab/genologics) module, genologicsR provides reference classes to manage tansactions with LIMS and to work with the xml data representing database entities (files, sample, artifacts, processes, field, etc.).

+ **LimsRefClass** provides uniform inteface for basic GET, PUT, POST, DELETE transactions as wells as hybrid queries and requests get_containers, get_samples, get_processes, get_artifacts, getByLimsid, etc.

```
lims <- Lims(configfile = "~/.clarityrc")
# Reference Class:"LimsRefClass"
#   Lims version: v2
#   Lims baseuri: http://scgc-clarity-dev.bigelow.org:8080/api/v2
#  Lims valid session: TRUE
```

+ **NodeRefClass** contains an instance of LimsRefClass as well as XML::xmlNode to provide uniform access to behaviors GET, PUT, POST etc. as well as access to attributes and child elements. The following inherit from NodeRefClass.

+ **ProcessRefClass** 

```
node <- lims$get_byLimsid('24-798', resource = "processes")[[1]]
P <- Process$new(node, lims)
# or equivalently
P <- lims$get_byLimsid('24-798', resource = "processes", asNode = TRUE)[[1]]
P
# Reference Class: "ProcessRefClass"
#    Reference Class:"LimsRefClass"
#      Lims version: v2
#      Lims baseuri: http://scgc-clarity-dev.bigelow.org:8080/api/v2
#      Lims valid session: TRUE
#   Node uri: http://scgc-clarity-dev.bigelow.org:8080/api/v2/processes/24-798
#   Node namespace: prc
#   Node children: type technician input-output-map field process-parameter
#   date-run: 
#   technician: Liz Fergusson
#   type: Dilution Step
```

+ **ContainerRefClass** 

```
node <- lims$get_byLimsid("27-1", resource = "containers")[[1]]
C <- Container$new(node, lims)
# or equivalently
C <- lims$get_byLimsid("27-1", resource = "containers", asNode = TRUE)[[1]]

C
# Reference Class: "ContainerRefClass"
#    Reference Class:"LimsRefClass"
#      Lims version: v2
#      Lims baseuri: http://scgc-clarity-dev.bigelow.org:8080/api/v2
#      Lims valid session: TRUE
#   Node uri: http://scgc-clarity-dev.bigelow.org:8080/api/v2/containers/27-1
#   Node namespace: con
#   Node children: name type occupied-wells placement state
#   name: BEN_1
#   type: 96 well plate
#   state: Populated
#   occupied wells: 7 (89 empty)
```

We can also leverage the faster batch resource by getting multiple nodes in one transaction. We have `retrieve`, `update` and `create` batch resources for container and sample while artifact and file have only `retrieve` and `update`.

```
# get containers by name
CC <- lims$get_containers(name = c("Liz_test_1", "Liz_test_2") )
CC
# $Liz_test_1
# Reference Class: "ContainerRefClass"
#    Reference Class:"LimsRefClass"
#      Lims version: v2
#      Lims baseuri: http://scgc-clarity-dev.bigelow.org:8080/api/v2
#      Lims valid session: TRUE
#   Node uri: http://scgc-clarity-dev.bigelow.org:8080/api/v2/containers/27-3
#   Node namespace: con
#   Node children: name type occupied-wells placement state
#   name: Liz_test_1
#   type: Tube
#   state: Populated
#   occupied wells: 1 (0 empty)
# 
# $Liz_test_2
# Reference Class: "ContainerRefClass"
#    Reference Class:"LimsRefClass"
#      Lims version: v2
#      Lims baseuri: http://scgc-clarity-dev.bigelow.org:8080/api/v2
#      Lims valid session: TRUE
#   Node uri: http://scgc-clarity-dev.bigelow.org:8080/api/v2/containers/27-4
#   Node namespace: con
#   Node children: name type occupied-wells placement state
#   name: Liz_test_2
#   type: Tube
#   state: Populated
#   occupied wells: 1 (0 empty)
```

+ **SampleRefClass**

```
# get a sample
node<- lims$get_byLimsid("FER101A1", resource = 'samples')[[1]]
S <- Sample$new(node = node, lims = lims)
# or equivalently
S <- lims$get_byLimsid("FER101A1", resource = 'samples', asNode = TRUE)[[1]]
S
# Reference Class: "SampleRefClass"
#    Reference Class:"LimsRefClass"
#      Lims version: v2
#      Lims baseuri: http://scgc-clarity-dev.bigelow.org:8080/api/v2
#      Lims valid session: TRUE
#   Node uri: http://scgc-clarity-dev.bigelow.org:8080/api/v2/samples/FER101A1
#   Node namespace: smp
#   Node children: name date-received project submitter artifact field
#   Sample name: Sample1
#   Sample type: NA
#   Sample date_received: 2015-06-19
#   Sample date_completed: NA
#   Sample biosource: 

# get samples by projectlimsid (or name, or projectname)
S <- lims$get_samples(projectlimsid = "FER101")
length(S)
[1] 96
```
