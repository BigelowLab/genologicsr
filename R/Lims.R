   
#' Reference class for Lims instance
#' @name LimsRefClass
#' @family Lims
NULL
LimsRefClass <- setRefClass('Lims',
   fields = list(
      version = 'character',
      baseuri = 'character',
      auth = 'ANY',
      handle = 'ANY',
      NSMAP = 'character')
)  


#' Print a pretty summary
NULL
LimsRefClass$methods(
   show = function(){
      cat("Reference Class:", methods::classLabel(class(.self)), "\n")
      cat("  version:", .self$version, "\n")
      cat("  baseuri:", .self$baseuri, "\n")
      cat("  valid_session:", .self$validate_session(), "\n")
   }) # show

#' Validate the session by testing the version
#' 
#' @name LimsRefClass_validate_session
#' @return logical, TRUE if OK
NULL
LimsRefClass$methods(
   validate_session = function(){
      ok <- TRUE
      x <- httr::GET(.self$baseuri, 
         .self$auth, 
         handle = .self$handle)
      if (httr::status_code(x) != 200) {
         warning("response has non-200 status code")
         print(x)
         ok <- FALSE
      }
      ok
   })


#' Verify a response and return xmlNode, possible an exception node
#' 
#' @param rsp httr::response object
#' @msg if an exception is encountered, attach this message (unless NULL)
#' @return XML::xmlNode
LimsRefClass$methods(
   check = function(rsp, msg = NULL){
      w <- httr::warn_for_status(rsp)
      if (!is.logical(w)) print(rsp)
      x <- XML::xmlRoot(content(rsp, type = "text/xml"))
      if (inherits(x, "try-error")){
         x <- .self$create_exception(message = "error parsing response content with xmlRoot")
      }
      invisible(x)
   }) # verify_response
    
#' Create an exception node
#' @param message chracter, some error message
#' @return XML::xmlNode
LimsRefClass$methods(
   create_exception = function(message = 'Unspecified exception'){
      x <- XML::newXMLNode("exception", 
         namespaceDefinitions = .self$NSMAP[['exc']], 
         namespace = 'exc')
      x <- XML::addChildren(x, kids = list(XML::newXMLNode("message", message)) )
      x
   }) #create_exception



#' GET a resource
#'
#' @name LimsRefClass_GET
#' @param uri character the uri to get
#' @param ... further arguments for httr::GET
#' @return XML::xmlNode - possibly an error node
NULL
LimsRefClass$methods(
   GET = function(uri, ...){
      r <- httr::GET(uri, 
         ..., 
         .self$auth,
         handle = handle)
     .self$check(r) 
   }) #GET


#' PUT a resource
#'
#' @family Lims
#' @param node xmlNode to put
#' @param ... further arguments for httr::PUT
#' @return xmlNode
NULL
LimsRefClass$methods(
   PUT = function(node, ...){
      if (missing(xmlNode)) stop("LimsRefClass$PUT node is required")
      uri <- splituri(xmlAttrs(node)["uri"])
      body <- gsub("\n", "", toString.XMLNode(node))
      r <- httr::PUT(uri,  
         ..., 
         body = body, 
         config = list(add_headers(content_type_xml)), 
         .self$auth,
         handle = .self$handle) 
      .self$check(r)
   }) # PUT


#' POST a resource
#'
#' @family Lims
#' @param node XML::xmlNode to POST
#' @param ... further arguments for httr::POST
#' @return XML::xmlNode
NULL
LimsRefClass$methods(
   POST = function(node, ...){
      if (missing(xmlNode)) stop("LimsRefClass$POST node is required")
      uri <- splituri(xmlAttrs(node)["uri"])
      body <- gsub("\n", "", toString.XMLNode(node))
      r <- httr::POST(uri, 
         ..., 
         body = body, 
         config = list(add_headers(content_type_xml)),
         .self$auth,
         handle = .self$handle) 
      .self$check(r)
   }) # POST
   
#' DELETE a resource
#' 
#' Typically this is a file (resource = 'files')
#' 
#' @family Lims
#' @param node XML::xmlNode to DELETE, generally a file node
#' @param ... further arguments for httr::DELETE
#' @return logical
NULL
LimsRefClass$methods(
   DELETE = function(node, ...){
      if (missing(node)) stop("LimsRefClass$DELETE node is required")
      uri <- splituri(xmlAttrs(node)["uri"])
      r <- httr::DELETE(uri, 
         ...,  
         .self$auth,
         handle = .self$handle)
      if (status_code(r) != 204){
         warn("LimsRefClass$DELETE unknown issue")
         print(r)
      }
      invisible(status_code(r) == 204)
   }) # POST

#' PUSH a file - not really a RESTfule action but a combination of steps
#' 
#' given and artifact node
#' if the artifact has a file then 
#'    DELETE the file resource
#' create an unresolved file resource
#' POST the unresolved file resource to get a resolved file resource
#' Upload the file (scp)
#' POST the resolved file resource again
#' return the resolved file resource
#      
#' @family Lims
#' @param node XML::xmlNode of the artifact to attach to 
#' @param ... further arguments for httr::GET/DELETE/POST
#' @param filename character, the fully qualified name of the file we are pushing
#' @return XML::xmlNode
NULL
LimsRefClass$methods(
   PUSH = function(node, ..., filename = ""){
      if (missing(xmlNode)) stop("LimsRefClass$PUSH node is required")
      if (!file.exists(filename[1])) stop("LimsRefClass$PUSH file not found:", filename[1])
      attached_to_uri <- splituri(xmlAttrs(node)["uri"])
      # if the artifact node has a file element
      # then we need to DELETE it
      if (is.null(node[["file"]]) == FALSE) {
         fileuri <- xmlAttrs(node[["file"]])["uri"]
         ok <- .self$DELETE(fileuri)
         if (!ok) {
            x <- create_exception_node(message = "LimsRefClass$PUSH: Unable to delete existing file")
            return(x)
         }
      }
      #' create an unresolved file resource
      unresolved_node <- create_file_node(attach_to_uri, filename[1])
      resolved_node <- .self$POST(unresolved_node)
      fileuri <- xmlValue(resolved_node[['content-location']])
      body <- httr::upload_file(filename[1])
      resolved_node <- httr::POST(resolved_node, body = body)
      
      
   }) # POST


#' Retrieve a resource by limsid
#' 
#' @name LimsefClass_getByLimsid
#' @param lismid character, one or more limsids
#' @param resource character, one resource to search, by default 'artifacts'
#' @return a list of XML::xmlNode objects
LimsRefClass$methods(
   getByLimsid = function(limsid, resource = 'artifacts', ...){
      uri <- file.path(.self$baseuri, resource[1], limsid)
      lapply(uri, .self$GET, ...)
   })

#### methods above
#### functions below


#' Instantiate a LimsRefClass object
#'
#' @param configfile character, the fully qualified path to the config file
#' @return a LimsRefClass instance or NULL
Lims <- function(configfile){
   if (missing(configfile)) stop("configfile is required")
   if (!file.exists(configfile[1])) stop("configfile not found:", configfile[1])
   x <- try(read_config(configfile[1]))
   if (inherits(x, "try-error")) stop("Error reading config file")
   
   X <- LimsRefClass$new()
   X$handle <- NULL
   X$field("version", get_config(x, "genologics", "VERSION", default = ""))
   buri <- get_config(x, "genologics", "BASEURI", default = "")
   if (nchar(buri) == 0) stop("base uri not found in config file")
   X$field("baseuri", file.path(buri, "api", X$version))
   X$field('auth', 
      authenticate(get_config(x, "genologics", "USERNAME", default = ""),
                   get_config(x, "genologics", "PASSWORD", default = "") 
      ) )
   X$field("handle", httr::handle(buri))
   X$NSMAP <- get_NSMAP()
   if (!X$validate_session()) {
      warning("API session failed validation")
      #return(NULL)
   } 
   X
}
