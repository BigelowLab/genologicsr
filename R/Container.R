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
      cat(prefix, "  name: ", .self$name, "\n", sep = "")
      cat(prefix, "  type: ", .self$type, "\n", sep = "")
      cat(prefix, "  state: ", .self$state, "\n", sep = "")
      cat(prefix, "  occupied wells: ", .self$occupied_wells(), sep = "")
   })  
   
#' Retrieve the occupied well count
#'
#' @family Container
#' @name ContainerRefNode_occupied_wells
#' @return numeric
NULL
ContainerRefClass$methods(
   occupied_wells = function(){
      nd <- .self$node[['occupied-wells']]
      if (!is.null(nd)) {
         x <- as.numeric(XML::xmlValue(nd))
      } else {
         x <- NA
      }
      x
   }) #occupied_wells
   

#' Retrieve a named vector of placements uri
#' 
#' @family Container
#' @name ContainerRefNode_get_placements
#' @return a named vector of placements
NULL
ContainerRefClass$methods(
   get_placements = function(wstyle = "A:1"){
      uri <- sapply(.self$node['placement'], function(x) XML::xmlAttrs(x)[['uri']])
      names(uri) <- sapply(.self$node['placement'], XML::xmlValue)
      invisible(uri)
   })

#' Retrieve a named list of artifact XML:xmlNode
#' 
#' @family Container
#' @name ContainerRefNode_get_artifacts
#' @return a named vector of artifact XML:xmlNode
NULL
ContainerRefClass$methods(
   get_artifacts = function(wstyle = "A:1"){
      uri <- .self$get_placements()
      SS <- .self$lims(file.path(.self$lims$baseuri, "artifacts", "batch", "retrieve")
   })