# Sample.R

#' A Sample representation that subclasses from NodeRefClass
#' 
#' @family Node
#' @field name character, name if any
#' @field type character, type if any
#' @field date_received character, "YYYY-mm-dd"
#' @field date_completed character, "YYYY-mm-dd"
#' @field biosource character, ummmm...
#' @include Node.R
#' @export
SampleRefClass <- setRefClass("SampleRefClass",
   contains = "NodeRefClass",
   fields = list(
      name = 'character',
      type = 'character',
      date_received = 'character',
      date_completed = 'character',
      biosource = 'character'
      ),
   methods = list(
      initialize = function(...){
         callSuper(...)
         .self$name = .self$get_name()
         .self$type = .self$get_type()
         .self$date_received =  get_childvalue(.self$node, 'date-received') 
         .self$date_completed = get_childvalue(.self$node, 'date-completed')
         .self$biosource = .self$get_biosource()
      })
   )

Sample <- getRefClass("SampleRefClass")
      

#' Show
#' 
#' @family Node Sample
#' @name SampleRefNode_show
NULL
SampleRefClass$methods(
   show = function(prefix = ""){
      callSuper(prefix = prefix)
      cat(prefix, "  Sample name: ", .self$name, "\n", sep = "")
      cat(prefix, "  Sample type: ", .self$type, "\n", sep = "")
      cat(prefix, "  Sample project: ", .self$project, "\n", sep = "")
      cat(prefix, "  Sample date_received: ", .self$date_received, "\n", sep = "")
      cat(prefix, "  Sample date_completed: ", .self$date_completed, "\n", sep = "")
      cat(prefix, "  Sample biosource: ", .self$get_biosource(), "\n", sep = "")
      cat(prefix, "  Sample artifact: ", .sefl$get_artifact(form = "uri"), "\n", sep = "")
   })  


#' POST is disallowed for samples
#' @family Sample
#' @name SampleRefNode_POST
NULL
SampleRefClass$methods(
   POST = function(){
      cat("SampleRefClass_POST in not a permitted transaction\n")
   })

#' DELETE is disallowed for samples
#' @family Sample
#' @name SampleRefNode_DELETE
NULL
SampleRefClass$methods(
   DELETE = function(){
      cat("SampleRefClass_DELETE in not a permitted transaction\n")
   })

   
#' Get Biosource description
#' 
#' @family Sample
#' @name SampleRefClass_get_biosource
#' @return character, possible empty
NULL
SampleRefClass$methods(
   get_biosource = function(){
      nd <- .self$node[['biosource']]
      if (!is.null(nd)) xmlAttrs(nd)[['name']] else ""
   })
 
  
#' Get artifact as uri or Node
#' 
#' @family Sample
#' @name SampleRefClass_get_artifact
#' @param form character either "Node" or "uri"
#' @return character of NodeRefClass, possibly "" or NULL
NULL
SampleRefClass$methods(
   get_artifact = function(form = c("Node", "uri")[2]){
      if (!.self$has_child("artifact")){
         if(form == "uri") {
            x <- ""
         } else {
            x <- NULL
         }
      } else {
         x <- xmlAttrs(.self$node[['artifact']])[['uri']]
         if (tolower(form) == "Node"){
            x <- .self$lims$GET(x, asNode = TRUE)
         }
      }
      invisible(x)
   })  
   
#' Get project as uri or Node
#' 
#' @family Sample
#' @name SampleRefClass_get_project
#' @param form character either "Node" or "uri"
#' @return character of NodeRefClass, possibly "" or NULL
NULL
SampleRefClass$methods(
   get_project = function(form = c("Node", "uri")[2]){
      if (!.self$has_child("project")){
         if(form == "uri") {
            x <- ""
         } else {
            x <- NULL
         }
      } else {
         x <- xmlAttrs(.self$node[['project']])[['uri']]
         if (tolower(form) == "Node"){
            x <- .self$lims$GET(x, asNode = TRUE)
         }
      }
      invisible(x)
   })   


########## methods above
########## functions below

#' Create a sample details node assembled from one or more sample XML::xmlNode
#' See \url{http://genologics.com/files/permanent/API/latest/data_smp.html#element_details}
#' 
#' @export
#' @param x one or more XML::xmlNode for sample or SampelRefClass objects
#' @param a sample details XML:xmlNode node
create_sample_details <- function(x){
   
   if (inherits(x, "SampleRefClass")){
      x <- lapply(x, "[[", "node")      
   }
   nm <- sapply(x, xmlName)
   if (!all(nm == "sample")) stop("create_sample_details: input nodes must be of type sample")
   
   newXMLNode("details",
      namespace = "smp",
      namespaceDefinitions = get_NSMAP()['smp'],
      .children = x)
   
   
}
