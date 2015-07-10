   
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
      cat(prefix, "  Lims version: ", .self$version, "\n", sep = "")
      cat(prefix, "  Lims baseuri: ", .self$baseuri, "\n", sep = "")
      cat(prefix, "  Lims valid session: ", .self$validate_session(), "\n", sep = "")
   }) # show


#' Build a uri staring with the baseuri
#'
#' @family Lims
#' @param ... one or more character segments to append to the base
#' @param base character, the base uri, if NULL then use this object's baseuri property
#' @return character uri
#' @examples 
#' \dontrun{
#'    my_uri <- ref$uri("containers")
#' }
NULL
LimsRefClass$methods(
   uri = function(..., base = NULL){
      stub = if(is.null(base)) .self$baseuri else base[1]
      file.path(stub, ...)
   })
   
#' Validate the session by testing the version
#' 
#' @family Lims
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
      if (!is.logical(w)) {
         print(rsp)
         print(content(rsp))
      }
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
         namespaceDefinitions = get_NSMAP()[['exc']], 
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
#' @param asNode logical, if TRUE return a class that inherits NodeRefClass 
#' @return XML::xmlNode - possibly an error node
NULL
LimsRefClass$methods(
   GET = function(uri=.self$baseuri, ..., depaginate = TRUE, asNode = TRUE){
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
      
      if (asNode) x <- parse_node(x, .self)
      
      invisible(x)
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
         body <- XML::toString.XMLNode(x)
      } else {
         stop("LimsRefClass$PUT: x must be xmlNode or NodeRefClass")
      }
      r <- httr::PUT(uri,  
         ..., 
         body = body, 
         httr::content_type_xml(), 
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
      if (missing(x) || !is_xmlNode(x) || ! inherits(x,"NodeRefClass")) 
         stop("LimsRefClass$POST x as NodeRefClass is required")
      r <- httr::POST(x$uri, 
         ..., 
         body = x$toString(), 
         httr::content_type_xml(),
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
      
      if (inherits(x, "NodeRefClass")){
         uri <- x$uri
         #body <- x$toString()
      } else if (is_xmlNode(x)) {
         uri <- trimuri(xmlAttrs(x)[['uri']])
         #body <- XML::toString.XMLNode(x)
      } else {
         stop("LimsRefClass$DELETE: x must be xmlNode or NodeRefClass")
      }
      r <- httr::DELETE(uri, 
         ...,  
         #body = body,
         #httr::content_type_xml(),
         .self$auth,
         handle = .self$handle)
      if (status_code(r) != 204){
         warn("LimsRefClass$DELETE unknown issue")
         print(r)
         print(content(r))
      }
      invisible(status_code(r) == 204)
   }) # POST

#' PUSH a file - not really a RESTfule action but a combination of steps
#' 
#' given an artifact node
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
      resolved_node <- httr::POST(resolved_node, body = list(file=body))
      
      
   }) # PUSH


#' Retrieve a resource by limsid
#' 
#' @family Lims 
#' @name LimsRefClass_get_byLimsid
#' @param lismid character, one or more limsids
#' @param resource character, one resource to search, by default 'artifacts'
#' @return a list of XML::xmlNode objects
NULL
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
#' @family Lims Container
#' @name LimsRefClass_get_containers
#' @param optional name a character vector of one or more names
#' @param optional type character of one or more container types ("384 well plate", etc)
#' @param optional state character of one or more contain states ("Discarded", "Populated",...)
#' @param optional last_modified a character vector of last modification date in YYYY-MM-DDThh:mm:ssTZD format
#' @return a named list of container XML::xmlNode
NULL
LimsRefClass$methods(
   get_containers = function(name = NULL, type = NULL, state = NULL,
   last_modified = NULL){
      resource <- 'containers'
      query = list()
      if (!is.null(name)) query[['name']] <- name
      if (!is.null(type)) query[['type']] <- type
      if (!is.null(state)) query[['state']] <- state
      if (!is.null(last_modified)) query[['last-modified']] <- last_modified
      if(length(query) == 0) 
         stop("LimsRefClass$get_containers please specify at leatst one or more of name, type, state or last_modified")
      query <- build_query(query)
      x <- .self$GET(file.path(.self$baseuri, resource), query = query)
      # lapply(xmlChildren(x) function(x) Container$new(x, .self))
      if (!is_exception(x)){
         uri <- sapply(XML::xmlChildren(x), function(x) xmlAttrs(x)[['uri']])
         x <- batch_retrieve(uri, .self, rel = 'containers')
         x <- lapply(x, function(x) Container$new(x, .self))
         names(x) <- sapply(x, '[[', 'name')
      }
      invisible(x)
   })

#' Get one or more samples using queries on name, projectlimsid, projectname
#' It is possible to also filter the query on UDF values but it may be easier to
#  do that after getting the samples - see \url{http://genologics.com/developer}
#' 
#' @family Lims Sample
#' @name LimsRefClass_get_samples
#' @param optional name a character vector of one or more names
#' @param optional projectlimsid character of one or more projectlimsid values
#' @param optional projectname character of one or more projectname values
#' @return a named list of container XML::xmlNode
NULL
LimsRefClass$methods(
   get_samples = function(name = NULL, projectlimsid = NULL, projectname = NULL){
      resource <- 'samples'
      query = list()
      if (!is.null(name)) query[['name']] <- name
      if (!is.null(projectlimsid)) query[['projectlimsid']] <- projectlimsid
      if (!is.null(projectname)) query[['projectname']] <- projectname
      if(length(query) == 0) 
         stop("LimsRefClass$get_samples please specify at least one or more of name, projectlimsid or projectname")
      query <- build_query(query)
      x <- .self$GET(.self$uri(resource), query = query)
      if (!is_exception(x)){
         uri <- sapply(XML::xmlChildren(x), function(x) xmlAttrs(x)[['uri']])
         x <- batch_retrieve(uri, .self, rel = 'samples')
         x <- lapply(x, function(x) Sample$new(x, .self))
         names(x) <- sapply(x, '[[', 'name')
      }
      invisible(x)
   })


#' Retrieve a list of ResearcherRefNodes or a data.frame of the good stuff
#' 
#' @family Lims Researcher
#' @name LimsRefClass_get_researchers
#' @param optional username character a vector of one or more user names like 'btupper' etc.
#' @return a list of  ResearcherRefClass, a data frame or NULL
NULL
LimsRefClass$methods(
   get_researchers = function(username = NULL, asDataFrame = TRUE){
   
      resource <- 'researchers'
      
      query = if(is.null(username)) NULL else build_query(list(username=username))
      
      RR <- lims$GET(.self$uri(resource), query = query)
      rr <- RR['researcher']
      if (length(rr) == 0) return(NULL)
      uri <- sapply(rr, function(x) xmlAttrs(x)[['uri']])
      x <- lapply(uri, function(x, lims = NULL) {
            lims$GET(x, asNode = TRUE)
         }, lims = .self)
      if (asDataFrame){
         x <- data.frame (name = sapply(x, function(x) x$name),
            username = sapply(x, function(x) x$username),
            initials = sapply(x, function(x) x$initials),
            email = sapply(x, function(x) x$email),
            stringsAsFactors = FALSE, 
            row.names = sapply(x, function(x) x$username))
      }
      invisible(x)
   })


#' Get one or more Processes - does not leverage /batch/retrieve resources
#' but provides similar behavior.
#'
#' @family Lims Process
#' @name LimsRefClass_get_processes
#' @param last_modified optional character vector in YYYY-MM-DDThh:mm:ssTZD format
#' @param type optional character of the process type
#' @param inputartifactlimsid optional character of an input artifact limsid
#' @param technamefirst optional technician's first name
#' @param technamelast optional technician's last name
#' @param projectname optional project name
#' @return a list of ProcessRefClass or NULL
LimsRefClass$methods(
   get_processes = function(last_modified = NULL, type = NULL, 
      inputartifactlimsid = NULL, technamefirst = NULL, technamelast = NULL,
      projectname = NULL){
      
      resources <- 'processes'
      
      query <- list()
      if (!is.null(last_modified)) query[["last-modified"]] <- last_modified
      if (!is.null(type)) query[["type"]] <- type
      if (!is.null(inputartifactlimsid)) query[["inputartifactlimsid"]] <- inputartifactlimsid
      if (!is.null(technamefirst)) query[["technamefirst"]] <- technamefirst
      if (!is.null(technamelast)) query[["technamelast"]] <- technamelast
      if (!is.null(projectname)) query[["projectname"]] <- projectname
      if (!is.null(query)) query <- build_query(query)
   
      x <- lims$GET(.self$uri(resource), query = query)
      if (length(xmlChildren(x))) return(NULL)
      
      uri <- sapply(x, function(x) xmlAttrs(x)[['uri']])
      x <- lapply(uri, function(x, lims = NULL) {
            lims$GET(x, asNode = TRUE)
         }, lims = .self)
      
      invisible(x)
   }) # get_processes

#' Get one or more nodes by uri by batch (artifacts, files, samples, containers only)
#'
#' @family Lims Node
#' @name LimsRefClass_batchretrieve
#' @param uri a vector of one or more uri for atomic entities in the GLS API
#' @param rel the relative name space into the "batch/retrieve" 
#' @param asNode logical, if TRUE parse to the appropriate node type
#' @param rm_dups logical, if TRUE then remove duplicates
#' @param ... further arguments for \code{batch_retrieve}
#' @return a list of XML::xmlNode or NodeRefClass objects
NULL
LimsRefClass$methods(
   batchretrieve = function(uri, rel = c("artifacts", "samples", "containers", "files")[1], 
      rm_dups = TRUE, asNode = TRUE, ...){
      if (!(rel[1] %in% c("artifacts", "samples", "containers", "files"))) 
         stop("LimsRefClass$batchrretrieve rel must be one of artifacts, files, samples or containers")
      x <- batch_retrieve(uri, .self, rel = rel[1], rm_dups = rm_dups, ...)   
      if (asNode) x <- lapply(x, parse_node, .self)
      invisible(x)
   })



#' Update one or more Nodes (artifacts, samples, containers only)
#'
#' @family Lims Node
#' @name LimsRefClass_batchupdate
#' @param x a list of one or mode XML::xmlNode of NodeRefClass
#' @param asNode logical, if TRUE return NodeRefClass objects otherwise XML::xmlNode
#' @param ... further arguments for httr::POST
#' @return a list of NodeRefClass or NULL
NULL

LimsRefClass$methods(
   batchupdate = function(x, asNode = TRUE, ...){
      if (!is.list(x)) x <- list(x)
      if (inherits(x[[1]], "NodeRefClass") ) {
         origx <- x  
         x <- lapply(x, "[[", node)
      }
      ok <- sapply(x, is_xmlNode)
      if (!all(ok)) 
         stop("LimsRefClass$batchupdate: inputs must inherit xmlNode or NodeRefClass")
      nm <- unique(sapply(x, xmlName))
      if (length(nm) > 1) 
         stop("LimsRefClass$batchupdate: all nodes must be of the same type - ", paste(nm, collapse = " "))
      if (!(nm %in% c("artifact", "sample", "container")))
         stop("LimsRefClass$batchupdate: only artifact, sample and container types have batch update")
      
      detail <- switch(nm,
         'artifact' = create_artifact_details(x),
         'container' = create_container_details(x),
         'sample' = create_sample_details(x),
         NULL)
      
      if (is.null(detail)) stop("LimsRefClass$batchupdate: only artifact, sample and container types have batch update")
      
      URI <- .self$uri(paste0(nm, "s/batch/update"))
      r <- httr::POST(URI, ..., body = xmlString(detail), 
         add_headers(c("Content-Type"="application/xml")),
         lims$auth, handle = lims$handle)
      
      x <- lims$check(r)
      if (!is_exception(x)) {
         rel <- paste0(nm, "s")
         uri <- sapply(x['link'], function(x) xmlAttrs(x)[['uri']])
         r <- batch_retrieve(uri, .self, , rel = paste0(nm, "s"), ...)
         if (asNode) r <- lapply(r, parse_node, .self)
      } else {
         r <- NULL
      }
      
      invisible(r)
   })

#' Create one or more Nodes (samples, containers only)
#'
#' @family Lims Node
#' @name LimsRefClass_batchcreate
#' @param x a list of one or mode XML::xmlNode of NodeRefClass
#' @param asNode logical, if TRUE return NodeRefClass objects otherwise XML::xmlNode
#' @param ... further arguments for httr::POST
#' @return a list of NodeRefClass or NULL
NULL
LimsRefClass$methods(
   batchcreate = function(x, asNode = TRUE, ...){
      if (!is.list(x)) x <- list(x)
      if (inherits(x[[1]], "NodeRefClass") ) {
         origx <- x  
         x <- lapply(x, "[[", node)
      }
      ok <- sapply(x, is_xmlNode)
      if (!all(ok)) 
         stop("LimsRefClass$batchupdate: inputs must inherit xmlNode or NodeRefClass")
      nm <- unique(sapply(x, xmlName))
      if (length(nm) > 1) 
         stop("LimsRefClass$batchupdate: all nodes must be of the same type - ", paste(nm, collapse = " "))
      if (!(nm %in% c("sample", "container")))
         stop("LimsRefClass$batchupdate: only sample and container types have batch update")
      
      detail <- switch(nm,
         'container' = create_container_details(x),
         'sample' = create_sample_details(x),
         NULL)
      
      if (is.null(detail)) stop("LimsRefClass$batchupdate: only artifact, sample and container types have batch update")
      
      URI <- .self$uri(paste0(nm, "s/batch/update"))
      r <- httr::POST(URI, ..., body = xmlString(detail), 
         add_headers(c("Content-Type"="application/xml")),
         lims$auth, handle = lims$handle)
      
      x <- lims$check(r)
      if (!is_exception(x)) {
         rel <- paste0(nm, "s")
         uri <- sapply(x['link'], function(x) xmlAttrs(x)[['uri']])
         r <- batch_retrieve(uri, .self, , rel = paste0(nm, "s"), ...)
         if (asNode) r <- lapply(r, parse_node, .self)
      } else {
         r <- NULL
      }
      
      invisible(r)
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
#' @param all logical, by default we remove duplicates set this to TRUE to retrieve all, ignored if \code{asList = FALSE}
#' @param ... further arguments
#' @return a list of NodeRefClass
batch_retrieve <- function(uri, lims,
   rel = c("artifacts", "containers", "files", "samples" )[1],
   resource = file.path(rel, "batch", "retrieve"), 
   asList = TRUE,
   rm_dups = TRUE, ...){
   
   if (length(uri) == 0) stop("batch_uri: uri has zero-length")
   
   # does the user want ALL including duplicates?
   # if so then save the IDs for later
   if (!rm_dups) limsid_all <- basename(uri)
   uri <- uri[!duplicated(uri)]

   # create new nodes for each uri requested
   linkNodes <- lapply(uri, 
      function(x, rel=rel, name = "link") {
         XML::newXMLNode(name = name, attrs = list(uri = x, rel=rel)) 
      }, rel = rel)
   
   # make the request node with uri request children
   batchNode <- XML::newXMLNode("links",
      namespace = "ri",
      namespaceDefinitions = c("ri" = "http://genologics.com/ri"),
      .children = linkNodes)
   
   URI <- file.path(lims$baseuri, resource)
   r <- httr::POST(URI, ..., body = xmlString(batchNode), 
      add_headers(c("Content-Type"="application/xml")),
      lims$auth, handle = lims$handle)
   x <- lims$check(r)
   if (!is_exception(x) && asList){
      singleName <- switch(rel,
         "artifacts" = "artifact",
         "containers" = "container",
         "samples" = "sample",
         "files" = "file",
         rel)
      nm <- switch(singleName,
         'artifact' = 'art',
         'container' = 'con',
         'sample' = 'smp',
         'file' = 'file')
         
      nmspc <- xmlNamespace(x)
      x <- x[singleName]
      # transfer the the xmlnamespace to each child node
      uristub = get_NSMAP()[[nm]]
      x <- lapply(x, function(x, 
            nm=structure(uristub, .Names = nm, class = "XMLNamespace")) {
                  XML::xmlNamespace(x) <- nm; 
                  x}, 
            nm = nmspc)
      if (rm_dups == FALSE) {
         # name each node
         names(x) <- sapply(x, function(x) XML::xmlAttrs(x)["limsid"])
         # rebuild the list with duplicates
         x <- x[limsid_all]
      }
   }
   invisible(x) 
} # batch_retrieve


#' Update a batch of XML::xmlNodes




#' Convert a node to an object inheriting from NodeRefClass 
#'
#' @family Lims Node
#' @param node XML::xmlNode
#' @param lims LimsRefClass object
parse_node <- function(node, lims){

   if (!is_xmlNode(node)) stop("assign_node: node must be XML::xmlNode")
   if (!inherits(lims, 'LimsRefClass')) stop("assign_node: lims must be LimsRefClass")
   
   nm <- xmlName(node)[1]
   switch(nm,
       'artifact' = Artifact$new(node, lims),
       'process' = Process$new(node, lims),
       'container' = Container$new(node, lims),
       'sample' = Sample$new(node,lims),
       'input-output-map' = InputOutputMap$new(node, lims),
       'researcher' = Researcher$new(node, lims),
       'file' = FileRefClass$new(node, lims),
       Node$new(node, lims))

}





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
