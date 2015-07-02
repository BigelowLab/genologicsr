# Researcher.R

ResearcherRefClass <- setRefClass("ResearcherRefClass",
   contains = "NodeRefClass",
      fields = list(
         username = 'character',
         name = 'character',
         email = 'character',
         initials = 'character'),
      methods = list(
         initialize = function(...){
            callSuper(...)
            .self$username = .self$get_username()
            .self$name = .self$get_name()
            .self$email = get_childvalue(.self$node, 'email')
            .self$initials = get_childvalue(.self$node, 'initials')
         })
      )
      
Researcher <- getRefClass("ResearcherRefClass")

#' Show
#' 
#' @family Node Researcher
NULL
ResearcherRefClass$methods(
   show = function(prefix = ""){
      callSuper(prefix = prefix)
      cat(prefix, "  Researcher name: ", .self$name, "\n", sep = "")
      cat(prefix, "  Researcher username: ", .self$username, "\n", sep = "")
      cat(prefix, "  Researcher email: ", .self$email, "\n", sep = "")
      cat(prefix, "  Researcher initials: ", .self$initials, "\n", sep = "")
   })  


#' Get the user's First Last name (overrides NodeRefClass_get_name)
#' 
#' @family Researcher
#' @name ResearcherRefClass_get_name
#' @return character of First Last
NULL
ResearcherRefClass$methods(
   get_name = function(){
      paste(get_childvalue(.self$node, "first-name"),
         get_childvalue(.self$node, "last-name") )
   }) #get_name

#' Get the user's username
#' 
#' @family Researcher
#' @name ResearcherRefClass_get_username
#' @return character of username
NULL
ResearcherRefClass$methods(
   get_username = function(){
      nd <- .self$node[['credentials']]
      if (is_xmlNode(nd)){
         x <- XML::xmlValue(.self$node[['credentials']][['username']])
      } else {
         x <- ""
      }
      x
   }) #get_name
   

#' Override the DELETE method to push user to use the GUI
#' 
#' @family Researcher
#' @name ResearcherRefClass_DELETE
#' @return NULL invisibly
NULL
ResearcherRefClass$methods(
   DELETE = function(){
      cat("ResearcherRefClass_DELETE please use the user interface to delete a user\n")
   }) #DELETE

#' Override the POST method to push user to use the GUI
#' 
#' @family Researcher
#' @name ResearcherRefClass_POST
#' @return NULL invisibly
NULL
ResearcherRefClass$methods(
   POST = function(){
      cat("ResearcherRefClass_POST is disabled\n")
   }) #POST
   
##### Methods above
##### Functions below


get_researchers <- function(lims, asNode = FALSE){
   if (missing(lims) || !inherits(lims, "LimsRefClass")) 
      stop("get_researchers: input must be LimsRefClass")
   
   RR <- lims$GET(lims$uri("researchers"))
   uri <- sapply(RR, function(x) xmlAttrs(x)[['uri']])
   x <- lapply(uri, function(x, lims = NULL) {
         lims$GET(x, asNode = TRUE)
      }, lims = lims)
   if (!asNode){
      x <- data.frame (name = sapply(x, function(x) x$name),
         username = sapply(x, function(x) x$username),
         initials = sapply(x, function(x) x$initials),
         email = sapply(x, function(x) x$initials),
         stringsAsFactors = FALSE, 
         row.names = sapply(x, function(x) x$username))
   }
   invisible(x)
}