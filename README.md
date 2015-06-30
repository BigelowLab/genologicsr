#### History

We created our own API wrapper in R to access the GLS API v1 using [curl](http://curl.haxx.se/) and [libxml2](http://xmlsoft.org/) via the R packages [RCurl](http://cran.r-project.org/web/packages/RCurl/index.html) and [XML](http://cran.r-project.org/web/packages/XML/index.html).  This R wrapper is privately distributed in the glsAPI package. The maintainer of the RCurl and XML packages is very busy, and while the packages are stable the update/fix cycle can be slow.  Of the two, XML has wide support from others and so it seems less orphan-like.

#### New direction

The arrival of [GLS API v2](http://genologics.com/developer) and Clarity LIMS it is appropriate to take stock of the resources available to us and make improvements.  To this end we have started to develop the genologicsR package model after the python [genologics](https://github.com/SciLifeLab/genologics) module.  We still use the XML package, but in lieue of RCurl we use the [httr](http://cran.r-project.org/web/packages/httr/index.html) package which provides a uniform interface to [RESTful](https://en.wikipedia.org/wiki/Representational_state_transfer) http transactions.  httr depends upon the [curl](http://cran.r-project.org/web/packages/curl/index.html) package which appears to be vigourously maintained.  httr and curl are maintained by people active with the RStudio project so the chances for longevity look bright.

#### genologicsR

Like the python [genologics](https://github.com/SciLifeLab/genologics) module, genologicsR provides reference classes to manage tansactions with LIMS and to work with the xml data representing database entities (files, sample, artifacts, processes, field, etc.).

+ **LimsRefClass** provides uniform inteface for basic GET, PUT, POST, DELETE transactions as wells as hybrid queries and requests get_containers, get_samples, get_processes, get_artifacts, getByLimsid, etc.
    
+ **NodeRefClass** contains an instance of LimsRefClass as well as XML::xmlNode to provide uniform access to behaviors GET, PUT, POST etc. as well as access to attributes and child elements. The following inherit from NodeRefClass.

++ **ProcessRefClass** 

++ **ContainerRefClass** 

++ **SampleRefClass**
