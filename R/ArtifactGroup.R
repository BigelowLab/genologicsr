# ArtifactGroup.R

#' An ArtifactGroup representation that subclasses from NodeRefClass
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
         .self$verbs <- c("GET", "BROWSE")
         .self$update()   
      },
   update = function(){
         callSuper(.self$node)
         .self$name = xml_value(.self$node[['name']])  
      },
    show = function(prefix = ""){
         callSuper(prefix = prefix)
         cat(prefix, "  Artifactgroup name: ", .self$name, "\n", sep = "")
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
         function(x) xml_atts(x)[['uri']])
      AA <- .self$lims$batchretrieve(uris,rel = 'artifacts', asNode = TRUE)
      invisible(AA)
   })


#' PUT is disallowed for artifact groups
#' @family ArtifactGroup
#' @name ArtifactGroupRefNode_PUT
NULL
ArtifactGroupRefClass$methods(
   PUT = function(){
      cat("ArtifactGroupRefClass_PUT in not a permitted transaction\n")
   })
   
#' POST is disallowed for artifact groups
#' @family ArtifactGroup
#' @name ArtifactGroupRefNode_POST
NULL
ArtifactGroupRefClass$methods(
   POST = function(){
      cat("ArtifactGroupRefClass_POST in not a permitted transaction\n")
   })

#' DELETE is disallowed for ArtifactGroup
#' @family ArtifactGroup
#' @name ArtifactGroupRefNode_DELETE
NULL
ArtifactGroupRefClass$methods(
   DELETE = function(){
      cat("ArtifactGroupRefClass_DELETE in not a permitted transaction\n")
   })
   