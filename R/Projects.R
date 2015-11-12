#' A Projects representation that subclasses from NodeRefClass  It is a container
#' for other projects.
#' 
#' @family Node
#' @include Node.R
#' @export
ProjectsRefClass <- setRefClass("ProjectsRefClass",
   contains = "NodeRefClass",
   methods = list( 
      initialize = function(...){
         callSuper(...)
         .self$verbs <- c('GET', 'POST', 'BROWSE')
         .self$update()   
      },
      show = function(prefix = ""){
         callSuper(prefix = prefix)
         nm <- .self$get_projects_uri()
         cat(prefix, "  Projects names:", paste(names(nm), collapse = ", "), "\n", sep = "")
          
      })
   )
   
#' Retrieve a list of project uri 
#'
#' @family Projects
#' @name ProjectsRefClass_get_projects_uri
#' @return a named character vector of URIs, possible empty
NULL
ProjectsRefClass$methods(
   get_projects_uri = function(){
      r <- ""
      if (is_xmlNode(.self$node)){
         p <- .self$node['project']
         if (!is.null(p)){
           r <- sapply(p, function(x) XML::xmlAttrs(x)[['uri']])
           names(r) <- sapply(p, function(x) XML::xmlValue(x))
         }
      } 
      invisible(r)
   })
   
#' Retrieve a list of projects
#'
#' @family Projects
#' @name ProjectsRefClass_get_projects
#' @param name character, one or more names of projects to get - if missing then all
#' @param form character, return either 'Node' or 'uri'
#' @return list of ProjectRefClass objects or NULL
NULL
ProjectsRefClass$methods(
   get_projects = function(name, form = c("Node", "uri")[2] ){
      x <- .self$get_projects_uri()
      if (!missing(name)) x <- URI[names(x) %in% name]
      if (length(x) == 0) return(NULL)
      
      if (tolower(form) == "node"){
         x <- lapply(x, function(x) {.self$lims$GET(x)})
      }
      invisible(x)   
   })   


#' PUT is disabled for ProjectsRefNodes
#' @family Projects
#' @name ProjectsRefClass_PUT
NULL
ProjectsRefClass$methods(
   PUT = function(){
      cat("ProjectsRefClass_PUT in not a permitted transaction\n")
   })

#' DELETE is disabled for ProjectsRefNodes
#' @family Projects
#' @name ProjectsRefClass_DELETE
NULL
ProjectsRefClass$methods(
   DELETE = function(){
      cat("ProjectsRefClass_DELETE in not a permitted transaction\n")
   })
      