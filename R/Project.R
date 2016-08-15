# Project.R

#' A Project representation that subclasses from NodeRefClass
#' 
#' @family Node
#' @field name character, name if any
#' @include Node.R
#' @export
ProjectRefClass <- setRefClass("ProjectRefClass",
   contains = "NodeRefClass",
   fields = list(
      name = "character"),
   methods = list( 
      initialize = function(...){
         callSuper(...)
         .self$verbs = c("PUT", "GET", "BROWSE", "ATTACH")
         .self$update()  
      },
   update = function(){
         callSuper(.self$node)
         if ("name" %in% names(XML::xmlChildren(.self$node))) {
            .self$name = xml_value(.self$node[['name']])
         } else {
            .self$name <- ""
         }
      },
   show = function(prefix = ""){
        callSuper(prefix = prefix)
        cat(prefix, "  Project name:", .self$name, "\n", sep = "")
        open_date <- .self$get_date("open")
        close_date <- .self$get_date("close")
        invoice_date <- .self$get_date("close")
        if (!is.null(open_date)) 
            cat(prefix, "  open-date:", open_date, "\n", sep = "")
        if (!is.null(close_date)) 
            cat(prefix, "  close-date:", close_date, "\n", sep = "")
        if (!is.null(invoice_date)) 
            cat(prefix, "   invoice-date:", invoice_date, "\n", sep = "")

      })
   )


#' POST is disallowed for projects - although see this...
#' \url{http://genologics.com/files/permanent/API/latest/rest.version.projects.limsid.html}
#' @family Project
#' @name ProjectRefNode_POST
NULL
ProjectRefClass$methods(
   POST = function(){
      cat("ProjectRefClass_POST in not a permitted transaction\n")
   })

#' DELETE is disallowed for Project
#' @family Project
#' @name ProjectRefNode_DELETE
NULL
ProjectRefClass$methods(
   DELETE = function(){
      cat("ProjectRefClass_DELETE in not a permitted transaction\n")
   })
   

#' Retrieve a date "open", "close" or "invoice"
#' 
#' @family project
#' @name ProjectRefClass_get_date
#' @param what character either 'open', 'close' or 'invoice'
#' @return a character date %Y-%m-%d or NULL
NULL
ProjectRefClass$methods(
    get_date = function(what = c("open", "close", "invoice")[1]){
    
        what = tolower(what[1])
        nd <- .self$node[[paste0(what, "-date")]]
        x <- if(!is.null(nd)) xml_value(nd) else NULL
        x
    })

#' Retrieve the Researcher assigned to the project
#' @family Project
#' @name ProjectRefClass_get_researcher
#' @param form character, either 'uri' or 'Node'
#' @return ResearcherRefClass node or NULL
NULL
ProjectRefClass$methods(
   get_researcher = function(form = c("uri", "Node")[2]){
      form <- tolower(form[1])
      x <- if(form == 'uri') "" else NULL
      if ("researcher" %in% names(XML::xmlChildren(.self$node))){
         x <- xml_atts(.self$node[['researcher']])
         if(form == 'node') x <- .self$lims$GET(x)
      }
      invisible(x)
   })

#' Retrieve the samples associated with a project
#'
#' This method may take a while depending upon the number of samples in the 
#' system.
#'
#' @family Project
#' @name ProjectRefClass_get_samples
#' @return a list of zero or more SampleRefClass or NULL
NULL
ProjectRefClass$methods(
   get_samples = function(){
      .self$lims$get_samples(projectlimsid = .self$limsid)
   })

#' Retrieve the artifacts associated with a project
#'
#' This method may take a while depending upon the number of samples in the 
#' system. The 'all' option is available but may be ill advised as it can be
#' very very slow.
#'
#' @family Project
#' @name ProjectRefClass_get_artifacts
#' @param what request either 'submitted' (default) or 'all'  sample artifacts
#' @param SS optional list of Samples in the Project.  If NULL then these
#'    are first retrieved which can be slow.
#' @return a list of list of zero or more SampleRefClass or NULL
NULL
ProjectRefClass$methods(
   get_artifacts = function(what = c('all', 'submitted')[2], SS = NULL){
      if (is.null(SS)) SS <- .self$get_samples()
      if (tolower(what) == 'all'){
         # this makes too big of a URL query ?name=foo&name=bar&...
         #SSlimsid <- sapply(SS, function(x) x$limsid)
         #AA <- .self$lims$get_artifacts(samplelimsid = SSlimsid)
         # so we iterate like this - who knows, maybe this is better as we 
         # retain one list element per sample (each element with one or more Artifacts)
         AA <- lapply(SS, function(x) x$get_all_artifacts())
      } else {
         AA <- sapply(SS, function(x) x$get_artifact(form = 'uri'))
         AA <- .self$lims$batchretrieve(AA, rel = 'artifacts')
      }
      AA
   })
      
#' Retrieve the containers associated with a project
#' 
#' @family Project
#' @name ProjectRefClass_get_containers
#' @param AA optional list of Artifacts in the Project.  If NULL then these
#'    are first retrieved which can be slow.
#' @return a list of zero or more ContainerRefClass or NULL
NULL
ProjectRefClass$methods(
   get_containers = function(AA = NULL){
      if (is.null(AA)) AA <- .self$get_artifacts(what = 'submitted')
      CC <- unique(sapply(unlist(AA), function(x) x$get_container(form = 'uri')))
      .self$lims$batchretrieve(CC, rel = 'containers')
   })
   


###### Methods above
###### Functions below

#' Create a project XML::xmlNode suitable for POSTing 
#' 
#' @export
#' @family Lims Project
#' @param name character project name (required)
#' @param open_date character, open date or NULL
#' @param close_date character, the close date or NULL
#' @param invoice_date character, invoice date or NULL
#' @param researcher character URI (required)
#' @return XML::xmlNode
create_project_node <- function(name = NULL, 
   open_date = format(as.POSIXct(Sys.time()), format = "%Y-%m-%d"),
   close_date = NULL,
   invoice_date = NULL,
   researcher = NULL){
   
      if (is.null(name)) stop("create_project_node name is required")
      if (is.null(researcher)) stop("create_project_node researcher uri is required")
      
      kids <- list(
         XML::newXMLNode("name", name[1]),
         XML::newXMLNode("researcher", attrs = list(uri=researcher[1])))
      
      if (!is.null(open_date))
         kids <- append(kids, XML::newXMLNode("open-date", open_date[1]) )
      if (!is.null(close_date) && !is.null(open_date)) 
         kids <- append(kids, XML::newXMLNode("close-date", close_date[1]) )
      if (!is.null(invoice_date) )
         kids <- append(kids, XML::newXMLNode("invoice-date", invoice_date[1]) )
      
      XML::newXMLNode('project',
         namespace = 'prj',
         namespaceDefinitions = get_NSMAP()['prj'],
         .children = kids)
      
   }