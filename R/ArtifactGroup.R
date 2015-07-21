# ArtifactGroup.R

#' An ArtifactGroup (aka Project) representation that subclasses from NodeRefClass
#' 
#' @family Node
#' @field name character, name if any
#' @include Node.R
#' @export
ArtifactGroupRefClass <- setRefClass("ArtifactGroupRefClass",
   contains = "NodeRefClass",
   fields = list(
      name = "character"),
   methods = list( 
      initialize = function(...){
         callSuper(...)
         .self$update()   
      },
   update = function(){
         callSuper(.self$node)
         .self$name = XML::xmlValue(.self$node[['name']])  
      })
   )

#' Retrieve a list ArtifactRefClass objects
#' 
#' @family ArtifactGroup
#' @name ArtifactGroupRefClass_get_artifacts
#' @return a named vector of ArtifactRefClass objects
NULL
ArtifactGroupRefClass$methods(
   get_artifacts = function(){
      uris <- sapply(.self$node['artifact'], 
         function(x) XML::xmlAttrs(x)[['uri']])
      AA <- .self$lims$batchretrieve(uris,rel = 'artifacts', asNode = TRUE)
      invisible(AA)
   })
