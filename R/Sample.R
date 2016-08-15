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
         .self$verbs <- c('GET', 'PUT', 'BROWSE')
      },
      
    update = function(...){
        callSuper(...)
        .self$name = .self$get_name()
        .self$type = .self$get_type()
        .self$date_received =  get_childvalue(.self$node, 'date-received') 
        .self$date_completed = get_childvalue(.self$node, 'date-completed')
        .self$biosource = .self$get_biosource()  
    }
      )
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
      cat(prefix, "  Sample date_received: ", .self$date_received, "\n", sep = "")
      cat(prefix, "  Sample date_completed: ", .self$date_completed, "\n", sep = "")
      cat(prefix, "  Sample biosource: ", .self$get_biosource(), "\n", sep = "")
      cat(prefix, "  Sample artifact: ", .self$get_artifact(form = "uri"), "\n", sep = "")
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
      # biosource element may be empty
      x <- ""
      nd <- .self$node[['biosource']]
      if (!is.null(nd)) {
         atts <- xml_atts(nd)
         if ( !is.null(atts) && ("name" %in% names(atts) )) x <- atts[['name']] 
      }
      return(x)
   })
 

#' Get ALL associated artifacts.  This can be painfully slow and probably is
#' not worth doing.
#' 
#' @family Sample
#' @name SampleRefClass_get_all_artifacts
#' @return list of ArtifactRefClass or NULL
NULL
SampleRefClass$methods(
   get_all_artifacts = function(){
      .self$lims$get_artifacts(samplelimsid = .self$limsid)
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
         if(tolower(form) == "uri") {
            x <- ""
         } else {
            x <- NULL
         }
      } else {
         x <- trimuri(xml_atts(.self$node[['artifact']])[['uri']])
         if (tolower(form) == "node") x <- .self$lims$GET(x, asNode = TRUE)
         
      }
      invisible(x)
   })  


#' Get the sample container as uri or NodeRefClass
#' 
#' @family Sample
#' @name SampleRefClass_get_container
#' @param form character either "uri" or "Node"
#' @return ContainerRefClass or uri (or NULL or "")
NULL
SampleRefClass$methods(
   get_container = function(form = c("Node", "uri")[1]){
      
      A <- .self$get_artifact(form = 'Node')
      if (is.null(A)) return(NULL)
      C <- A$get_container(form = form)
      invisible(C)    
   })


   
#' Get project as uri or Node
#' 
#' @family Sample
#' @name SampleRefClass_get_project
#' @param form character either "Node" or "uri"
#' @return character of NodeRefClass, possibly "" or NULL
NULL
SampleRefClass$methods(
   get_project = function(form = c("Node", "uri")[1]){
      if (!.self$has_child("project")){
         if(form == "uri") {
            x <- ""
         } else {
            x <- NULL
         }
      } else {
         x <- xml_atts(.self$node[['project']])[['uri']]
         if (tolower(form) == "node"){
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
#' @param x one or more XML::xmlNode for sample or SampleRefClass objects
#' @param a sample details XML:xmlNode node
create_samples_details <- function(x){
   
   if (inherits(x, "SampleRefClass")){
      x <- lapply(x, "[[", "node")      
   }
   nm <- sapply(x, xml_name)
   if (!all(nm %in% c("sample", "samplecreation") )) stop("create_samples_details: input nodes must be of type sample")
   
   XML::newXMLNode("details",
      namespace = "smp",
      namespaceDefinitions = get_NSMAP()['smp'],
      .children = x)
} # create_sample_details

#' Create a sample XML::xmlNode suitable for POSTing 
#' 
#' @export
#' @family Lims Sample
#' @param name character sample name (required)
#' @param project_uri character uri of project (required)
#' @param container_uri character uri of the container (required)
#' @param well character location on the well such as 'A:1' (required)
#' @param date_received optional character date in the form 'YYYY-MM-DD'
#' @param date_completed optional character date in the form 'YYYY-MM-DD'
#' @return XML::xmlNode
create_sample_node <- function(name = NULL, 
   project_uri = NULL, container_uri = NULL, well = NULL,
   date_received = NULL, date_completed = NULL){
   
      if (is.null(name)) stop("create_sample_node name is required")
      if (is.null(project_uri)) stop("create_sample_node project_uri is required")
      if (is.null(container_uri)) stop("create_sample_node container_uri is required")
      if (is.null(well)) stop("create_sample_node well is required")
      
      nmsp <- 'smp'
            
      kids <- list(
         XML::newXMLNode("name", name[1]),
         XML::newXMLNode("project", attrs = list(uri = project_uri[1])),
         XML::newXMLNode("location",
            .children = list(
               XML::newXMLNode("container", attrs = list( "uri" = container_uri[1])),
               XML::newXMLNode("value", well[1])
               )
            )
         )
      if (!is.null(date_received)){
         kids <- c(kids, XML::newXMLNode("date-received", date_received[1]))
      }
      if (!is.null(date_completed)){
         kids <- c(kids, XML::newXMLNode("date-completed", date_completed[1]))
      }      
      XML::newXMLNode('samplecreation',
         namespace = nmsp,
         namespaceDefinitions = get_NSMAP()[nmsp],
         .children = kids)
      
} # create_container_node

