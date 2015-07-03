# Container.R

ContainerRefClass <- setRefClass("ContainerRefClass",
   contains = "NodeRefClass",
   fields = list(
      name = "character",
      state = "character",
      type = "character"),
   methods = list( 
      initialize = function(...){
         callSuper(...)
         .self$name = xmlValue(.self$node[['name']]) 
         .self$type = xmlAttrs(.self$node[['type']])[['name']]
         .self$state = xmlValue(.self$node[['state']])    
      })
   )
   
Container <- getRefClass("ContainerRefClass")

#' Show
#' 
#' @family Node Process
NULL
ContainerRefClass$methods(
   show = function(prefix = ""){
      callSuper(prefix = prefix)
      cat(prefix, "  Container name: ", .self$name, "\n", sep = "")
      cat(prefix, "  Container type: ", .self$type, "\n", sep = "")
      cat(prefix, "  Container state: ", .self$state, "\n", sep = "")
      cat(prefix, "  Container occupied wells: ", .self$n_occupied(), 
         " (", .self$n_empty(), " empty)\n", sep = "")
   })  

#' POST is disallowed for containers
#' @family Container
#' @name ContainerRefNode_POST
NULL
ContainerRefClass$methods(
   POST = function(){
      cat("ContainerRefClass_POST in not a permitted transaction\n")
   })
   
#' Retrieve the occupied well count
#'
#' @family Container
#' @name ContainerRefNode_n_occupied
#' @return numeric
NULL
ContainerRefClass$methods(
   n_occupied = function(){
      nd <- .self$node[['occupied-wells']]
      if (!is.null(nd)) {
         x <- as.numeric(XML::xmlValue(nd))
      } else {
         x <- NA
      }
      x
   }) #occupied_wells
   
#' Retrieve the empty well count
#'
#' @family Container
#' @name ContainerRefNode_n_empty
#' @return numeric
NULL
ContainerRefClass$methods(
   n_empty = function(){
      .self$n_wells() - .self$n_occupied()
   }) #empty_wells
   
#' Retrieve the well count (occupied + empty)
#'
#' @family Container
#' @name ContainerRefNode_n_wells
#' @return numeric
NULL
ContainerRefClass$methods(
   n_wells = function(){
      if ( (length(.self$type) == 0) || (nchar(.self$type) == 0) ) return(NA)
      switch(.self$type,
         '384 well plate' = 384,
         '96 well plate' = 96,
         '12 well plate' = 12,
         1)
   }) #n_wells  

#' Retrieve a named vector of placements uri
#' 
#' @family Container
#' @name ContainerRefNode_get_placements
#' @return a named vector of placements, possibly an empty charcater vector
NULL
ContainerRefClass$methods(
   get_placements = function(){
      if ( !.self$has_child("placement") ) return(charcater())
      puri <- sapply(.self$node['placement'], function(x) XML::xmlAttrs(x)[['uri']])
      names(puri) <- sapply(.self$node['placement'], XML::xmlValue)
      invisible(puri)
   })

#' Retrieve a named list of artifact XML:xmlNode or ArtifactRefClass objects
#' 
#' @family Container
#' @name ContainerRefNode_get_artifacts
#' @param asNode logical if TURE cast the result to ArtifactRefClass
#' @return a named vector of artifact XML:xmlNode or ArtifactRefClass objects
NULL
ContainerRefClass$methods(
   get_artifacts = function(){
      puri <- .self$get_placements() 
      AA <- .self$lims$batchretrieve(puri,rel = 'artifacts', asNode = TRUE)
      names(AA) <- sapply(.self$node['placement'], XML::xmlValue)
      invisible(AA)
   })
   

#' Retrieve a named list of artifact XML:xmlNode or SampleRefClass
#' 
#' @family Container
#' @name ContainerRefNode_get_samples
#' @param asNode logical if TURE cast the result to SampleRefClass
#' @return a named vector of sample XML:xmlNode or SampleRefClass objects
NULL
ContainerRefClass$methods(
   get_samples = function(asNode = TRUE){
      puri <- .self$get_placements()
      AA <- .self$lims$batchretrieve(puri, rel = 'artifacts', asNode = asNode)
      suri <- sapply(AA, function(x) x$get_sample(form = "uri"))
      SS <- .self$lims$batchretrieve(suri, rel = 'samples', asNode = asNode)
      names(SS) <- sapply(.self$node['placement'], XML::xmlValue)
      invisible(SS)
   })
   
