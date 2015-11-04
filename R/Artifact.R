# Artifact.R

#' An Artifact representation that sublcasses from NodeRefClass
#' 
#' @family Node
#' @field name character, name if any
#' @field type character, type if any
#' @include Node.R
#' @export
ArtifactRefClass <- setRefClass("ArtifactRefClass",
   contains = "NodeRefClass",
      fields = list(
         name = 'character',
         type = 'character'),
   methods = list(
      initialize = function(...){
         callSuper(...)
         .self$name = .self$get_name()
         .self$type = .self$get_type()
      })
   )

Artifact <- getRefClass("ArtifactRefClass")



#' Show
#' 
#' @family Node Artifact
#' @name ArtifactRefNode_show
NULL
ArtifactRefClass$methods(
   show = function(prefix = ""){
      callSuper(prefix = prefix)
      cat(prefix, "  Artifact name: ", .self$name, "\n", sep = "")
      cat(prefix, "  Artifact type: ", .self$type, "\n", sep = "")
      cat(prefix, "  Artifact parent-process: ", .self$get_parent_process(form = "uri"), "\n", sep = "")
      cat(prefix, "  Artifact sample: ", .self$get_sample(form = "uri"), "\n", sep = "")
      cat(prefix, "  Artifact location: ", .self$location(), "\n", sep = "")
      cat(prefix, "  Artifact qc-flag: ", .self$qc_flag(), "\n", sep = "")
      cat(prefix, "  Artifact working-flag: ", .self$working_flag(), "\n", sep = "")
   })  


#' POST is disallowed for artifacts
#' @family Artifact
#' @name ArtifactRefNode_POST
NULL
ArtifactRefClass$methods(
   POST = function(){
      cat("ArtifactRefClass_POST in not a permitted transaction\n")
   })

#' DELETE is disallowed for artifacts
#' @family Artifact
#' @name ArtifactRefNode_DELETE
NULL
ArtifactRefClass$methods(
   DELETE = function(){
      cat("ArtifactRefClass_DELETE in not a permitted transaction\n")
   })
   

#' Get the artifact container as uri or NodeRefClass
#' 
#' @family Artifact
#' @name ArtifactRefClass_get_container
#' @param form character either "uri" or "Node"
#' @return ContainerRefClass or uri (or NULL or "")
NULL
ArtifactRefClass$methods(
   get_container = function(form = c("Node", "uri")[1]){
      
      if (.self$has_child("location")){
      
         x <- trimuri(XML::xmlAttrs(.self$node[['location']][['container']])[['uri']])
         if (tolower(form[1]) == "node") x <- .self$lims$GET(x, asNode = TRUE)
         
      } else {
      
         x <- if(tolower(form[1]) == "uri") "" else NULL
      }
      
      invisible(x)    
   })


#' Get the name of the well in the form A:1
#' 
#' @family Artifact
#' @name ArtifactRefClass_get_wellname
#' @return character (possibly "")
NULL
ArtifactRefClass$methods(
   get_wellname = function(){
      if (.self$has_child("location")) {
         x <- XML::xmlValue(.self$node[['location']][['value']])
      } else {
         x <- ""
      }
      x
   })

#' Get a pretty string representing the location
#'
#' @family Artifact 
#' @name ArtifactRefClass_location
#' @return charcater of containerLimsid_location
NULL
ArtifactRefClass$methods(
   location = function(){
      if (.self$has_child("location")){
         con <- XML::xmlAttrs(.self$node[['location']][['container']])[['limsid']]
         plc <- XML::xmlValue(.self$node[['location']][['value']])
         r <- paste(basename(con), plc, sep = "_")
      } else {
         r <- ""
      }
      r
   })

#' Get the qc flag
#' 
#' @family Artifact
#' @name ArtifactRefClass_qc_flag
#' @return charcater of containerLimsid_location
NULL
ArtifactRefClass$methods(
   qc_flag = function(){
      get_childvalue(.self$node, "qc-flag")
   })
   
#' Get the working flag
#' 
#' @family Artifact
#' @name ArtifactRefClass_working_flag
#' @return charcater of containerLimsid_location
NULL
ArtifactRefClass$methods(
   working_flag = function(){
      get_childvalue(.self$node, "working-flag")
   })
   
 
#' Get the parent_process assocated with this artifact
#'
#' @family Artifact
#' @name ArtifactRefClass_get_parent_process
#' @param form character flag of type to return "Node" or "uri"
#' @return XML::xmlNode (or NULL) or the uri (or "")
ArtifactRefClass$methods(
   get_parent_process = function(form = c("Node", "uri")[2]){
      if (!.self$has_child("parent-process")){
         x <- switch(tolower(form),
            "uri" = "",
            NULL)
         return(x)
      }
      thisuri <- trimuri(XML::xmlAttrs(.self$node[["parent-process"]])[['uri']])
      if (tolower(form == "uri")){
         x <- thisuri
      } else {
         x <- .self$lims$GET(thisuri, asNode = TRUE)
      }
      invisible(x)
   })  
  
  
#' Get the artifact group assocated with this artifact
#'
#' @family Artifact ArtifactGroup
#' @name ArtifactRefClass_get_artifact_group
#' @param form character flag of type to return "Node" or "uri" or "name"
#' @return XML::xmlNode (or NULL) or the uri (or "")
ArtifactRefClass$methods(
   get_artifact_group = function(form = c("Node", "uri", "name")[2]){
      if (!.self$has_child("artifact-group")){
         x <- switch(tolower(form),
            "uri" = "",
            "name" = "",
            NULL)
         return(x)
      }
      atts <- XML::xmlAttrs(.self$node[["artifact-group"]])
      thisuri <- trimuri(atts[['uri']])
      thisname <- atts[['name']]
      x <- switch(tolower(form),
         "uri" = thisuri,
         "node" = self$lims$GET(thisuri, asNode = TRUE),
         "name" = thisname)
      invisible(x)
   })
      
#' Get the sample  associated with this artifact
#'
#' @family Artifact
#' @name ArtifactRefClass_get_sample
#' @param form character flag of type to return "xmlNode" or "uri"
#' @return XML::xmlNode (or NULL) or the uri (or "")
ArtifactRefClass$methods(
   get_sample = function(form = c("Node", "uri")[2]){
      if (!.self$has_child("sample")){
         x <- switch(tolower(form[1]),
            "uri" = "",
            NULL)
         return(x)
      }
      thisuri <- trimuri(XML::xmlAttrs(.self$node[["sample"]])[['uri']])
      if (tolower(form == "uri")){
         x <- thisuri
      } else {
         x <- .self$lims$GET(thisuri, asNode = TRUE)
      }
      invisible(x)
   }) 
   
#### methods above
#### functions below


#' Create an artifacts links node from uris suitable for batch operations
#' See \url{http://genologics.com/files/permanent/API/latest/rest.version.artifacts.batch.retrieve.html}
#' 
#' @export
#' @param x one or more URI
#' @return a links node
create_artifacts_links <- function(x){
   
   kids <- lapply(x, function(x) {
      XML::newXMLNode("link", attrs = list(uri = x, rel = 'artifacts'))
   }
   
   XML::newXMLNode("links", namespace = "ri",
      namespaceDefinitions = get_NSMAP()['ri'],
      .children = x)
}

#' Create a artifacts details node assembled from one or more artifacts XML::xmlNode
#' See \url{http://genologics.com/files/permanent/API/latest/data_art.html#element_details}
#' 
#' @export
#' @param x one or more XML::xmlNode for artifacts or ArtifactRefClass objects
#' @param a artifacts details XML:xmlNode node
create_artifacts_details <- function(x){
   
   if (!is.list(x)) x <- list(x)
   
   if (inherits(x[[1]], "ArtifactRefClass")){
      x <- lapply(x, "[[", "node")      
   }
   
   nm <- sapply(x, XML::xmlName)
   if (!all(tolower(nm) == "artifact")) stop("create_artifact_details: input nodes must be of type artifact")
   XML::newXMLNode("details",
      namespace = "art",
      namespaceDefinitions = get_NSMAP()[c('art', 'ri', 'udf', 'file', 'con')],
      .children = x)
} # create_containers_details
 