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
            .self$verbs <- c('GET', 'PUT', 'DELETE', 'BROWSE')
            .self$username = .self$get_username()
            .self$name = .self$get_name()
            .self$email = get_childvalue(.self$node, 'email')
            .self$initials = get_childvalue(.self$node, 'initials')
         })
      )
      
Researcher <- getRefClass("ResearcherRefClass")


#' Update the object fields
#' 
#' @name ResearcherRefClass_update
#' @family Researcher
#' @param ... further arguments for \code{NodeRefClass$update()}
NULL
ResearcherRefClass$methods(
   update = function(...){
      callSuper(...)
      .self$username = .self$get_username()
      .self$name = .self$get_name()
      .self$email = get_childvalue(.self$node, 'email')
      .self$initials = get_childvalue(.self$node, 'initials')
   })

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
      cat(prefix, "  Researcher credentials: ", .self$get_credentials(), "\n", sep = "")
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


#' Get the credentials as a string.  If the researcher
#' is uncredentialed then 'none' is returned.
#'
#' @family Researcher
#' @name ResearcherRefClass_get_credentials
#' @name sep character, the separator to use when pasting
#' @return character vector
NULL
ResearcherRefClass$methods(
   get_credentials = function(sep = ' '){
      
      nd <- .self$node[['credentials']]
      if (is_xmlNode(nd)){
         role <- sapply(nd['role'], function(x) xml_atts(x)[['roleName']])
         x <- paste(unname(role), collapse = sep)
      } else {
         x <- 'none'
      }
      return(x)
   }) # get_credentials

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
         x <- xml_value(.self$node[['credentials']][['username']])
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


#' Create a researcher XML::xmlNode suitable for POSTing 
#' 
#' @export
#' @family Lims Researcher
#' @param firstname character researcher first name (required)
#' @param lastname character researcher last name (required)
#' @param email character researcher email (required)
#' @param username character if credentailed the required
#' @param password character if credentailed then required
#' @param initials exactly 3 alphanumeric characters, if credentialed then required
#' @param role character vector of one or more roles
#' @param account_locked logical
#' @param lab character URI of the lab
#' @return XML::xmlNode
create_researcher_node <- function(firstname = NULL, lastname = NULL,
   email = NULL, username = NULL, password = NULL, initials = NULL,
   role = NULL, account_locked = NULL, lab = NULL){

      if (is.null(firstname)) stop("create_researcher_node firstname is required")
      if (is.null(lastname)) stop("create_researcher_node lastname is required")
      if (is.null(email)) stop("create_researcher_node email is required")

      nmsp <- c('udf','ri','res')
            
      kids <- list(
         XML::newXMLNode("firstname", firstname[1]),
         XML::newXMLNode("lastname", lastname[1]),
         XML::newXMLNode("email", email[1]))
      
      if (!is.null(initials)) kids <- base::append(kids, XML::newXMLNode("initials", initials[1]))
      if (!is.null(lab)) {
         labnode <- XML::newXMLNode("initials")
         XML::xmlAttrs(labnode) <- c("uri" = lab[1])
         kids <- base::append(kids, labnode)
      }
      
      creds <- XML::newXMLNode("credentials")
      if (!is.null(role)){
         allowed <- c("systemadministrator","administrator","labtech", "webclient")
         if (!all(role %in% allowed)){
           stop(paste("Allowed rolls are only:", paste(allowed, collapse = " ")))
         }
         creds <- XML::newXMLNode("credentials")
         k <- list()
         for (r in role) k <- append(k, XML::newXMLNode("role", name = r))
         if (!is.null(username)) k <- base::append(k, XML::newXMLNode("username", username[1]))
         if (!is.null(password)) k <- base::append(k, XML::newXMLNode("password", password[1]))
         if (!is.null(account_locked)) k <- base::append(k, 
            XML::newXMLNode("account-locked", tolower(as.character(account_locked[1]))))
            
         creds <- XML::addChildren(creds, kids = k)
         kids <- base::append(kids, creds)
      }
      
      
      XML::newXMLNode('researcher',
         namespace = nmsp,
         namespaceDefinitions = get_NSMAP()[nmsp],
         .children = kids)
      
} # create_container_node


