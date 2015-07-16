# Researcher.R

#' An Researcher representation that sublcasses from NodeRefClass
#' 
#' @family Node
#' @field character, username the user name
#' @field character, name the user's full name in 'First Last' form
#' @field email character, user's email
#' @field initials character, user's initials
#' @include Node.R
#' @export
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
#' @name ResearcherRefClass_show
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
