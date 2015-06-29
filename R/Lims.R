   
#' Reference class for Lims instance
#' @name LimsRefClass
#' @family Lims
NULL
LimsRefClass <- setRefClass('LimsRefClass',
   fields = list(
      version = 'character',
      baseuri = 'character',
      auth = 'ANY',
      handle = 'ANY')
)  


#' Print a pretty summary
#' @param prefix character - perhaps number of spaces to prefix the output 
NULL
LimsRefClass$methods(
   show = function(prefix = ""){
      cat(prefix, "Reference Class:", methods::classLabel(class(.self)), "\n", sep = "")
      cat(prefix, "  version: ", .self$version, "\n", sep = "")
      cat(prefix, "  baseuri: ", .self$baseuri, "\n", sep = "")
      cat(prefix, "  valid_session: ", .self$validate_session(), "\n", sep = "")
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
      x <- try(XML::xmlRoot(content(rsp, type = "text/xml")))
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
#' @param ... further arguments for httr::GET including \code{query} list
#' @param depaginate logical, if TRUE then pool paginated nodes into one
#' @return XML::xmlNode - possibly an error node
NULL
LimsRefClass$methods(
   GET = function(uri=.self$baseuri, ..., depaginate = TRUE){
      x <- httr::GET(uri, 
         ..., 
         .self$auth,
         handle = handle)
      
      x <- .self$check(x) 
      if ( !is_exception(x) && ("next-page" %in% names(x))  && depaginate ){
         uri <- xmlAttrs(x[['next-page']])[['uri']]
         doNext <- TRUE
         while(doNext){
            y <- .self$check(httr::GET(uri, .self$auth, handle = handle))
            children <- !(names(y) %in% c("previous-page", "next-page"))
            if (any(children)) x <- addChildren(x, kids = y[children])
            doNext <- "next-page" %in% names(y)
            if (doNext) uri <- xmlAttrs(y[["next-page"]])[["uri"]]
         } # doNext while loop
         x <- removeChildren(x, kids = x["next-page"]) 
      }
      return(x)
   }) #GET


#' PUT a resource
#'
#' @family Lims
#' @param x xmlNode to put
#' @param ... further arguments for httr::PUT
#' @return xmlNode
NULL
LimsRefClass$methods(
   PUT = function(x, ...){
      if (missing(x)) stop("LimsRefClass$PUT: node is required")
      
      if (inherits(x, "NodeRefClass")){
         uri <- x$uri
         body <- x$toString()
      } else if (is_xmlNode(x)) {
         uri <- trimuri(xmlAttrs(x)[['uri']])
         body <- toString(x)
      } else {
         stop("LimsRefClass$PUT: x must be xmlNode or NodeRefClass")
      }
      r <- httr::PUT(uri,  
         ..., 
         body = body, 
         content_type_xml(), 
         .self$auth,
         handle = .self$handle) 
      .self$check(r)
   }) # PUT


#' POST a resource
#'
#' @family Lims
#' @param x XML::xmlNode to POST
#' @param ... further arguments for httr::POST
#' @return XML::xmlNode
NULL
LimsRefClass$methods(
   POST = function(x, ...){
      if (missing(x)) stop("LimsRefClass$POST node is required")
      r <- httr::POST(x$uri, 
         ..., 
         body = x$toString(), 
         content_type_xml(),
         .self$auth,
         handle = .self$handle) 
      .self$check(r)
   }) # POST
   
#' DELETE a resource
#' 
#' Typically this is a file (resource = 'files')
#' 
#' @family Lims
#' @param x XML::xmlNode to DELETE, generally a file node
#' @param ... further arguments for httr::DELETE
#' @return logical
NULL
LimsRefClass$methods(
   DELETE = function(x, ...){
      if (missing(x)) stop("LimsRefClass$DELETE node is required")
      uri <- trimuri(xmlAttrs(x)["uri"])
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
#' @param x XML::xmlNode of the artifact to attach to 
#' @param ... further arguments for httr::GET/DELETE/POST
#' @param filename character, the fully qualified name of the file we are pushing
#' @return XML::xmlNode
NULL
LimsRefClass$methods(
   PUSH = function(x, ..., filename = ""){
      if (missing(x)) stop("LimsRefClass$PUSH node is required")
      if (!file.exists(filename[1])) stop("LimsRefClass$PUSH file not found:", filename[1])
      attached_to_uri <- trimuri(xmlAttrs(x)["uri"])
      # if the artifact node has a file element
      # then we need to DELETE it
      if (is.null(x[["file"]]) == FALSE) {
         fileuri <- xmlAttrs(x[["file"]])["uri"]
         ok <- .self$DELETE(fileuri)
         if (!ok) {
            e <- create_exception_node(message = "LimsRefClass$PUSH: Unable to delete existing file")
            return(e)
         }
      }
      #' create an unresolved file resource
      unresolved_node <- create_file_node(attach_to_uri, filename[1])
      resolved_node <- .self$POST(unresolved_node)
      fileuri <- xmlValue(resolved_node[['content-location']])
      body <- httr::upload_file(filename[1])
      resolved_node <- httr::POST(resolved_node, body = body)
      
      
   }) # PUSH


#' Retrieve a resource by limsid
#' 
#' @name LimsRefClass_get_byLimsid
#' @param lismid character, one or more limsids
#' @param resource character, one resource to search, by default 'artifacts'
#' @return a list of XML::xmlNode objects
LimsRefClass$methods(
   get_byLimsid = function(limsid, 
      resource = c("artifacts", "artifactgroups", "containers", "labs", "instruments", 
         "processes", "processtemplates", "projects", "researchers", "samples", 
         "configuration/udfs", "configuration/udts", "files")[1], ...){
      uri <- file.path(.self$baseuri, resource[1], limsid)
      lapply(uri, .self$GET, ...)
   })


#' Get one or more containers by name, state, etc
#' 
#' @name LimsRefClass_get_containers
#' @param name a character vector of one or more names
#' @param type character of one or more container types ("384 well plate", etc)
#' @param state character of one or more contain states ("Discarded", "Populated",...)
#' @param last_modified a character vector of last modification dates. 
#' @return a named list of container XML::xmlNode
LimsRefClass$methods(
   get_containers = function(name = NULL, type = NULL, state = NULL,
   last_modified = NULL, resource = "containers" ){
      query = list()
      if (!is.null(name)) query[['name']] <- name
      if (!is.null(type)) query[['type']] <- type
      if (!is.null(state)) query[['state']] <- state
      if (!is.null(last_modified)) query[['last-modified']] <- last_modified     
      x <- .self$GET(file.path(.self$baseuri, resource), query = query)
      lapply(xmlChildren(x) function(x) Container$new(x, .self))
   })


#### methods above
#### functions below

#' Retrieve a batch resource
#'
#' @export
#' @param uri character vector of one or more uri
#' @param relative resource name
#' @param resource character
#' @param asList logical, if TRUE return a named list of Artifacts
#' @param all logical, by default we remove duplicates
batch_retrieve <- function(uri, lims,
   rel = c("artifacts", "containers", "files", "samples" )[1],
   resource = file.path(rel, "batch", "retrieve"), 
   asList = TRUE,
   all = FALSE, ...){
   
   if (length(uri) == 0) stop("batch_uri: uri has zero-length")
   
   # does the user want ALL including duplicates?
   # if so then save the IDs for later
   if (all) limsid_all <- basename(uri)
   uri <- uri[!duplicated(uri)]

   # create new nodes for each uri requested
   linkNodes <- lapply(uri, 
      function(x, rel=rel, name = "link") {
         XML::newXMLNode(name = name, attrs = list(uri = x, rel=rel)) 
      } )
   
   # make the request node with uri request children
   batchNode <- XML::newXMLNode("links",
      namespace = "ri",
      namespaceDefinitions = c("ri" = "http://genologics.com/ri"),
      .children = linkNodes)
   
   URI <- file.path(lims$baseuri, resource)
   r <- httr::POST(URI, ..., body = xmlString(batchNode), lims$auth, handle = lims$handle)
   x <- lims$check(r)
   if (!is_exception(x)){
   
   
   }
   invisible(x) 
   
} # batch_retrieve


#' Instantiate a LimsRefClass object
#'
#' @export
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
   if (!X$validate_session()) {
      warning("API session failed validation")
      #return(NULL)
   } 
   X
}
