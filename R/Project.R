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
         .self$update()  
      },
   update = function(){
         callSuper(.self$node)
         if ("name" %in% names(XML::xmlChildren(.self$node))) {
            .self$name = XML::xmlValue(.self$node[['name']])
         } else {
            .self$name <- ""
         }
      },
   show = function(prefix = ""){
      callSuper(prefix = prefix)
      cat(prefix, "  Project name:", .self$name, "\n", sep = "")
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
   


#' Retrieve the Researcher assigned to the project
#' @family Project
#' @name ProjectRefClass_get_researcher
#' @return ResearcherRefClass node or NULL
NULL
ProjectRefClass$methods(
   get_researcher = function(){
      x <- NULL
      if ("researcher" %in% names(XML::xmlChildren(.self$node))){
         URI <- XML::xmlAttrs(.self$node[['researcher']])
         x <- .self$lims$GET(URI)
      }
      invisible(x)
   })

###### Methods above
###### Functions below

#' Create a project XML::xmlNode suitable for POSTing 
#' 
#' @export
#' @family Lims Project
#' @param name character project name (required)
#' @param open_date character, by default the current date
#' @param close_date character, the close date
#' @param invoice_date character
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
         XML::newXMLNode("researcher", researcher[1]),
         XML::newXMLNode("open-date", open_date[1]) )
      
      if (!is.null(close_date)) 
         kids <- append(kids, XML::newXMLNode("close-date", close_date[1]) )
      if (!is.null(invoice_date))
         kids <- append(kids, XML::newXMLNode("invoice-date", invoice_date[1]) )
      
      XML::newXMLNode('project',
         namespace = 'prj',
         namespaceDefinitions = get_NSMAP()['prj'],
         .children = kids)
      
   }