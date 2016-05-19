   
#' Reference class for Lims instance
#'
#' @family Lims
#' @field version character Genologics LIMS version
#' @field baseuri character the base uri
#' @field auth httr::authenticate object for LIMS
#' @field fileauth httr::authenticate object for filestore
#' @field handle httr::handle object - accessor function provides a fresh handle
#'   for each transaction, but see \url{http://rstudio-pubs-static.s3.amazonaws.com/64194_2282137119ca48e1893054091456fe43.html#on-re-using-handles}
#' @include Node.R
#' @export
LimsRefClass <- setRefClass('LimsRefClass',
   fields = list(
      version = 'character',
      baseuri = 'character',
      encoding = 'character',
      auth = 'ANY',
      fileauth = 'ANY',
      handle = 'ANY',
      timeout = 'integer')
)  


#' Print a pretty summary
#' @name LimsRefClass_show Sample
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
#' @name LimsRefClass_uri
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
         encoding = .self$encoding, 
         handle = .self$handle,
         .self$auth)
      if (httr::status_code(x) != 200) {
         warning("response has non-200 status code")
         print(x)
         ok <- FALSE
      }
      ok
   })


#' Retrieve the user and password
#'
#' @family Lims
#' @name LimsRefClass_userpwd
#' @param what character to specify which authorization ('lims' or 'file')
#'    by default lims is returned
#' @return character vector of [username, password] or NULL
NULL
LimsRefClass$methods(
   userpwd = function(what = c("lims", "file")){
   Auth <- switch(tolower(what[1]),
      'file' = .self$fileauth,
      .self$auth)
      
   if (is.null(Auth)){
      up <- NULL
   } else {
      # we can't depend upon httr providing the same format of attributes
      if ("userpwd" %in% names(Auth)){
         up <- strsplit(Auth[['userpwd']],":", fixed = TRUE)[[1]]
      } else if ("options" %in% names(Auth)) {
         up <- strsplit(Auth[['options']][['userpwd']],":", fixed = TRUE)[[1]]
      } else{
         cat("LimsRefClass$userpwd: unable to retrieve credentials\n")
         up <- NULL
      }
   }
   invisible(up)
   })
   
   
#' Verify a response and return xmlNode, possible an exception node
#' 
#' @name LimsRefClass_check
#' @family Lims
#' @param rsp httr::response object
#' @param msg if an exception is encountered, attach this message (unless NULL)
#' @return XML::xmlNode
NULL
LimsRefClass$methods(
   check = function(rsp, msg = NULL){
      # with httr 1.1 httr::warn_for_status no longer returns a logical
      # instead it returns either the response (no warning) or a condition error
      # see https://cran.r-project.org/web/packages/httr/news.html
      # so now we switch to httr::http_error()
      #w <- httr::warn_for_status(rsp) 
      w <- httr::http_error(rsp)
      if (!is.logical(w)) {
         print(rsp)
         print(httr::content(rsp, as = "text", encoding = .self$encoding))
      }
      
      x <- try(httr::content(rsp, as = "text", encoding = .self$encoding))
      if (inherits(x, 'try-error')){
         x <- .self$create_exception(message = "error extracting response content")
         return(invisible(x))
      }
      x <- try(XML::xmlTreeParse(x, asText = TRUE, 
         encoding = .self$encoding, useInternalNodes = TRUE,
         fullNamespaceInfo = TRUE))
      if (inherits(x, "try-error")){
         x <- .self$create_exception(message = "error with xmlTreeParse")
         return(invisible(x))
      }
      
      x <- try(XML::xmlRoot(x))
      if (inherits(x, "try-error")){
         x <- .self$create_exception(message = "error parsing response content with xmlRoot")
      }
      
      invisible(x)
   }) # verify_response
  
#' BROWSE a URI in a browser if in interactive session
#'
#' @family Lims
#' @name LimsRefClass_BROWSE
#' @param x XML::xmlNode or NodeRefClass
#' @param ... further arguments for httr::BROWSE
NULL
LimsRefClass$methods(
   BROWSE = function(x, ...){
   
      stopifnot(interactive())
      
      if (is_xmlNode(x)){
         uri <- trimuri(xml_atts(x)[['uri']])
      } else if (inherits(x, 'character')) {
         uri <- x[1]
      } else if (inherits(x, 'NodeRefClass')){
         uri <- x$uri
      }
      
      httr::BROWSE(uri, 
         ..., 
         .self$auth)
   })
     
#' Create an exception node
#'
#' @name LimsRefClass_create_exception
#' @param message chracter, some error message
#' @param asNode logical if TRUE return ExceptionRefClass
#' @return XML::xmlNode of ExceptionRefClass
LimsRefClass$methods(
   create_exception = function(message = 'Unspecified exception',
        asNode = FALSE){
      #x <- XML::newXMLNode("exception", 
      #   namespaceDefinitions = get_NSMAP()[['exc']], 
      #   namespace = 'exc')
      #x <- XML::addChildren(x, kids = list(XML::newXMLNode("message", message)) )
      x <- create_exception_node(message = message)
      if (asNode) x <- parse_node(x, .self)
      x
   }) #create_exception



#' GET a resource, a wrapper around get_uri
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
      x <- get_uri(uri, .self, ..., depaginate = depaginate)
      if (asNode) x <- parse_node(x, .self) 
      invisible(x)
   }) #GET


#' PUT a resource
#'
#' @family Lims
#' @name LimsRefClass_PUT
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
         uri <- trimuri(xml_atts(x)[['uri']])
         body <- xml_string(x)
      } else {
         stop("LimsRefClass$PUT: x must be xmlNode or NodeRefClass")
      }
      r <- httr::PUT(uri,  
         ..., 
         body = body, 
         httr::content_type_xml(), 
         handle = .self$handle,
         .self$auth) 
      .self$check(r)
   }) # PUT


#' POST a resource
#'
#' @family Lims
#' @name LimsRefClass_POST
#' @param x XML::xmlNode to or NodeRefClass POST
#' @param uri character if NULL taken from \code{x}
#' @param asNode logical, if TRUE return a class that inherits NodeRefClass.
#'    This should be FALSE for batch processing. 
#' @param ... further arguments for httr::POST
#' @return XML::xmlNode or NodeRefClass
NULL
LimsRefClass$methods(
   POST = function(x, uri = NULL, asNode = FALSE, ...){
      if (missing(x)) 
         stop("LimsRefClass$POST x as XML::xmlNode or NodeRefClass is required")
         
      if (inherits(x, 'NodeRefClass')){
         if (is.null(uri)) uri <- x$uri
         body <- x$toString()
      } else {
         if (is.null(uri)) uri <- trimuri(xml_atts(x)[['uri']])
         body <- XML::toString.XMLNode(x)
      }
      r <- httr::POST(uri, 
         ..., 
         body = body, 
         httr::content_type_xml(),
         handle = .self$handle,
         .self$auth) 
      r <- .self$check(r)
      if (asNode) r <- try(parse_node(r, .self))
      r
   }) # POST
   
#' DELETE a resource
#' 
#' Typically this is a file (resource = 'files')
#' 
#' @family Lims
#' @name LimsRefClass_DELETE
#' @param x NodeRefClass, XML::xmlNode or character uri to DELETE, generally a file node
#' @param ... further arguments for httr::DELETE
#' @return logical
NULL
LimsRefClass$methods(
   DELETE = function(x, ...){
      if (missing(x)) stop("LimsRefClass$DELETE node is required")
      
      if (inherits(x, "NodeRefClass")){
         uri <- x$uri
      } else if (is_xmlNode(x)) {
         uri <- trimuri(xml_atts(x)[['uri']])
      } else if (inherits(x, 'character')) {
         uri <- trimuri(x)
      } else {
         stop("LimsRefClass$DELETE: x must be xmlNode, character, or NodeRefClass")
      }
      r <- httr::DELETE(uri, 
         ...,
         handle = .self$handle,
         .self$auth)
      if (httr::status_code(r) != 204){
         warning("LimsRefClass$DELETE unknown issue")
         print(r)
         print(httr::content(r))
      }
      invisible(httr::status_code(r) == 204)
   }) # POST

#' PUSH a file - not really a RESTful action but a combination of steps
#' 
#' given an artifact node and a filename
#' if the artifact has a file then 
#'    DELETE the file resource
#' create an unresolved file resource
#' POST the unresolved file resource to 'glsstore' to get a resolved file resource
#' Upload the file (scp, cp, or curl)
#' POST the resolved file resource to 'files'
#' return the resolved file resource
#      
#' @family Lims
#' @name LimsRefClass_PUSH
#' @param x ArtifactRefClass of the artifact to attach to 
#' @param ... further arguments for httr::GET/DELETE/POST
#' @param filename character, the fully qualified name of the file we are pushing
#'  Note that the caller must specify filename = 'some/file/name' explicitly.
#' @param use character the type of file transfer to use: duck, scp, cp or curl
#' @return XML::xmlNode or FileRefClass
NULL
LimsRefClass$methods(
   PUSH = function(x, ..., filename = "", 
      use = c("duck", "scp", "cp", "curl")[2]){
      
      stopifnot(inherits(x, 'ArtifactRefClass'))
      
      if (!file.exists(filename[1])) stop("LimsRefClass$PUSH file not found:", filename[1])
      
      attached_to_uri <- trimuri(x[["uri"]])
      
      # if the artifact node has a file element
      # then we need to DELETE it
      if ( !is.null(x$node[["file"]]) ) {
         fileuri <- xml_atts(x$node[["file"]])["uri"]
         ok <- .self$DELETE(fileuri, ...)
         if (!ok) {
            e <- create_exception(message = "LimsRefClass$PUSH: Unable to delete existing file", asNode = TRUE)
            return(e)
         }
      }
      # create an unresolved file resource
      unresolved_node <- create_file_node(attached_to_uri, filename[1])
      # POST it
      uri <- .self$uri("glsstorage")
      body <- xmlString(unresolved_node)
      rbefore<- httr::POST(uri,
         body = body,
         httr::content_type_xml(),
         .self$auth)
      rbefore <- .self$check(rbefore)  
      
      if (is_exception(rbefore)){ return(rbeforer)}
      resolved_node <- parse_node(rbefore, .self)
      
      
      # now we copy the file over...
      use <- tolower(use[1])
      dst <- resolved_node[['content_location']]
      up <- strsplit(.self$fileauth$options[['userpwd']], ":", fixed = TRUE)[[1]]
      puri <- httr::parse_url(resolved_node[['content_location']])

      ok <- 1
      if (use == "scp"){
          # first make the directory if it doesn't already exist
         MKDIR <- paste('ssh',
            paste0(up[1],'@',puri[['hostname']]), 
            shQuote(paste('mkdir -p', paste0("/", dirname(puri[['path']]) ) )) )
         ok <- system(MKDIR)
         if (ok == 0){
            # https://kb.iu.edu/d/agye
            # scp /path/to/source/file.txt dvader@deathstar.com:/path/to/dest/file.txt
            cmd <- paste('scp -q', filename[1], 
                paste0(up[[1]], "@", puri[['hostname']], ":/", puri[['path']] ))
            ok <- system(cmd)
         } else {
            e <- create_exception(message = "LimsRefClass$PUSH: Unable create destintion path", asNode = TRUE)
            return(e)
         }
      } else if (use == "cp"){
         MKDIR <- paste('mkdir -p', paste0("/", dirname(puri[['path']]) ) )
         ok <- system(MKDIR)
         if (ok == 0){
             cmd <- paste("cp", shQuote(filename[1]),
                paste0("/", puri[['path']]) )
             ok <- system(cmd)
         } else {
            e <- create_exception(message = "LimsRefClass$PUSH: Unable create destintion path", asNode = TRUE)
            return(e)
         }
      } else if (use == "curl"){
         cmd <- paste("curl --ftp-create-dirs",
            "-u", .self$fileauth[['options']][['userpwd']],
            "-T", filename[1],
            resolved_node[['content_location']])
         ok <- system(cmd)   
      } else if (use == 'duck'){
         up <- strsplit(.self$fileauth$options[['userpwd']], ":", fixed = TRUE)[[1]]
         ok <- duck_upload(filename[1], resolved_node[['content_location']],
            username = up[[1]], password = up[[2]])
      }
      if (ok != 0) {
         e <- create_exception(message = "LimsRefClass$PUSH: unable to upload file", asNode = TRUE)
         return(e)
      }
      
      uri <- .self$uri("files")
      body <- resolved_node$toString() 
      rafter <- httr::POST(uri,
         body = body,
         httr::content_type_xml(),
         .self$auth)
      rafter <- .self$check(rafter)  
      
      if (is_exception(rafter)){ return(rafter)}
      parse_node(rafter, .self)
   }) # PUSH


#' ATTACH a file - not really a RESTful action but a combination of steps
#'
#' Differs from PUSH as this is not placed into a genealogical placeholder.
#' but is simple attached to 'Files' tab if such exists as it does for Process
#' and Project.  Thus there is no DELETE involved like there might be with
#' a PUSH.
#' 
#' given an [Project,Process] node and a filename
#' create an unresolved file resource
#' POST the unresolved file resource to 'glsstore' to get a resolved file resource
#' Upload the file (scp, cp, or curl)
#' POST the resolved file resource to 'files'
#' return the resolved file resource
#      
#' @family Lims
#' @name LimsRefClass_ATTACH
#' @param x ProcessRefClass or ProjectRefClass to attach to 
#' @param filename character, the fully qualified name of the file we are pushing
#'  Note that the caller must specify filename = 'some/file/name' explicitly.
#' @param use character the type of file transfer to use: duck, scp, cp or curl
#' @return FileRefClass, NULL or ExceptionRefClass
NULL
LimsRefClass$methods(
   ATTACH = function(x,filename = "", 
      use = c("duck", "scp", "cp", "curl")[2]){
      
      stopifnot(!any(c(inherits(x, 'ArtifactRefClass'), inherits(x, 'ProjectRefClass'))))
      
      if (!file.exists(filename[1])) stop("LimsRefClass$ATTACH file not found:", filename[1])
      
      attached_to_uri <- trimuri(x[["uri"]])
      
      # create an unresolved file resource
      unresolved_node <- create_file_node(attached_to_uri, filename[1])
      # POST it
      uri <- .self$uri("glsstorage")
      body <- xmlString(unresolved_node)
      rbefore<- httr::POST(uri,
         body = body,
         httr::content_type_xml(),
         .self$auth)
      rbefore <- .self$check(rbefore)  
      
      if (is_exception(rbefore)){ return(rbeforer)}
      resolved_node <- parse_node(rbefore, .self)
      
      
      # now we copy the file over...
      use <- tolower(use[1])
      dst <- resolved_node[['content_location']]
      up <- strsplit(.self$fileauth$options[['userpwd']], ":", fixed = TRUE)[[1]]
      puri <- httr::parse_url(resolved_node[['content_location']])

      ok <- 1
      if (use == "scp"){
          # first make the directory if it doesn't already exist
         MKDIR <- paste('ssh',
            paste0(up[1],'@',puri[['hostname']]), 
            shQuote(paste('mkdir -p', paste0("/", dirname(puri[['path']]) ) )) )
         ok <- system(MKDIR)

         # https://kb.iu.edu/d/agye
         # scp /path/to/source/file.txt dvader@deathstar.com:/path/to/dest/file.txt
         cmd <- paste('scp -q', filename[1], 
            paste0(up[[1]], "@", puri[['hostname']], ":/", puri[['path']] ))
         ok <- system(cmd)
      } else if (use == "cp"){
         MKDIR <- paste('mkdir -p', paste0("/", dirname(puri[['path']]) ) )
         ok <- system(MKDIR)
         cmd <- paste("cp", filename[1],
            paste0("/", puri[['path']]) )
         ok <- system(cmd)
      } else if (use == "curl"){
         cmd <- paste("curl --ftp-create-dirs",
            "-u", .self$fileauth[['options']][['userpwd']],
            "-T", filename[1],
            resolved_node[['content_location']])
         ok <- system(cmd)   
      } else if (use == 'duck'){
         up <- strsplit(.self$fileauth$options[['userpwd']], ":", fixed = TRUE)[[1]]
         ok <- duck_upload(filename[1], resolved_node[['content_location']],
            username = up[[1]], password = up[[2]])
      }
      if (ok != 0) {
         # now what?
      }
      
      uri <- .self$uri("files")
      body <- resolved_node$toString() 
      rafter <- httr::POST(uri,
         body = body,
         httr::content_type_xml(),
         .self$auth)
      rafter <- .self$check(rafter)  
      
      if (is_exception(rafter)){ return(rafter)}
      parse_node(rafter, .self)
   }) # ATTACH


#' Retrieve a resource by limsid
#' 
#' @family Lims 
#' @name LimsRefClass_get_byLimsid
#' @param lismid character, one or more limsids
#' @param resource character, one resource to search, by default 'artifacts'
#' @return a list of NodeRefClass objects
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
#' @return a named list of ContainerRefClass or NULL
NULL
LimsRefClass$methods(
   get_containers = function(name = NULL, type = NULL, state = NULL,
   last_modified = NULL){
      resource <- 'containers'
      queryl = list()
      if (!is.null(name)) queryl[['name']] <- name
      if (!is.null(type)) queryl[['type']] <- type
      if (!is.null(state)) queryl[['state']] <- state
      if (!is.null(last_modified)) queryl[['last-modified']] <- last_modified
      if(length(queryl) == 0) 
         stop("LimsRefClass$get_containers please specify at least one or more of name, type, state or last_modified")
      query <- build_query(queryl)
      x <- .self$GET(file.path(.self$baseuri, resource), query = query, asNode = FALSE)
      if (!is_exception(x)){
         if (length(XML::xmlChildren(x))==0) return(NULL)
         uri <- sapply(XML::xmlChildren(x), function(x) xml_atts(x)[['uri']])
         x <- batch_retrieve(uri, .self, rel = 'containers')
         x <- lapply(x, function(x) ContainerRefClass$new(x, .self))
         names(x) <- sapply(x, '[[', 'name')
      }
      invisible(x)
   })


#' Get the container type(s) in the system
#' 
#' 
#' @family Lims Container
#' @name LimsRefClass_get_containertypes
#' @param name a character vector of one or more container type names
#' @return a named list of ContainerTypeRefClass objects or NULL
NULL
LimsRefClass$methods(
   get_containertypes = function(name = NULL){
      queryl = list()
      if (!is.null(name)) queryl[['name']] <- name
      query <- build_query(queryl)
      x <- .self$GET(.self$uri("containertypes"), query = query, 
         depaginate = TRUE, asNode = FALSE)
      if (!is_exception(x) && length(x['container-type']) > 0){
         uris <- sapply(x['container-type'], function(x) xml_atts(x)[['uri']])
         names(uris) <- sapply(x['container-type'], function(x) xml_atts(x)[['name']])
         x <- lapply(uris, function(x) .self$GET(x))
      } else {
         x <- NULL
      }

      invisible(x)
   })


#' Get artifact group(s) in the system
#' 
#' 
#' @family Lims
#' @name LimsRefClass_get_artifactgroups
#' @param artifactgroup a character vector of one or more artifact group names
#' @return a named list of ArtifactGroupTypeRefClass objects or NULL
NULL
LimsRefClass$methods(
   get_artifactgroups = function(artifactgroup = NULL){
      queryl = list()
      if (!is.null(artifactgroup)) queryl[['artifactgroup']] <- artifactgroup
      query <- build_query(queryl)
      x <- .self$GET(.self$uri("artifactgroups"), query = query, 
         depaginate = TRUE, asNode = FALSE)
      if (!is_exception(x) && length(x['artifactgroup']) > 0){
         uris <- sapply(x['artifactgroup'], function(x) xml_atts(x)[['uri']])
         #names(uris) <- sapply(x['artifactgroup'], function(x) xml_atts(x)[['name']])
         x <- lapply(uris, function(x) .self$GET(x))
         names(x) <- sapply(x, "[[", "name")
      } else {
         x <- NULL
      }

      invisible(x)
   })

#' Get one or more artifacts using queries on name, type, process-type, working-flag
#' qc-flag, sample-name, samplelimsid, containername, containerlimsid, reagent-label
#'
#' @family Lims Artifact
#' @name LimsRefClass_get_artifacts
#' @param name one or more artifact names or NULL to ignore
#' @param type character one or more character types or NULL to ignore
#' @param process_type character parent process type or NULL to ignore
#' @param working_flag character 'true' or 'false'  or NULL to ignore
#' @param qc_flag character on of UNKNOWN, PASSED, FAILED, CONTINUE or NULL to ignore
#' @param sample_name character one or more submitted sample names or NULL to ignore
#' @param samplelimsid character one or more submitted sample limsid  or NULL to ignore
#' @param artifactgroup character one or more experiment names  or NULL to ignore
#' @param container_name character one or more container names or NULL to ignore
#' @param containerlimsid character one or more container limsid  or NULL to ignore 
#' @param reagent-label character one or more reagent names or NULL to ignore
#' @return a list of ArtifactRefClass objects or NULL
NULL
LimsRefClass$methods(
   get_artifacts = function(name = NULL, type = NULL, process_type = NULL,
       working_flag = NULL,qc_flag = NULL,sample_name = NULL,samplelimsid = NULL,
       artifactgroup = NULL,container_name = NULL,containerlimsid = NULL,
       reagent_label = NULL){
       
      resource <- 'artifacts'
      query = list()
      if (!is.null(name)) query[['name']] <- name
      if (!is.null(type)) query[['type']] <- type
      if (!is.null(process_type)) query[['process-type']] <- process_type
      if (!is.null(working_flag)) query[['working-flag']] <- working_flag
      if (!is.null(qc_flag)) query[['qc-flag']] <- qc_flag
      if (!is.null(sample_name)) query[['sample-name']] <- sample_name
      if (!is.null(samplelimsid)) query[['samplelimsid']] <- samplelimsid
      if (!is.null(artifactgroup)) query[['artifactgroup']] <- artifactgroup
      if (!is.null(container_name)) query[['container-name']] <- container_name
      if (!is.null(containerlimsid)) query[['containerlimsid']] <- containerlimsid
      if (!is.null(reagent_label)) query[['reagent-label']] <- reagent_label
      if(length(query) == 0) 
         stop("LimsRefClass$get_artifacts please specify at least one or more of search parameters")
      query <- build_query(query)
      
      x <- .self$GET(.self$uri(resource), query = query, asNode = FALSE) 
      if (!is_exception(x)){
         if (length(XML::xmlChildren(x))==0) return(NULL)
         uri <- sapply(XML::xmlChildren(x), function(x) xml_atts(x)[['uri']])
         x <- batch_retrieve(uri, .self, rel = 'artifacts')
         x <- lapply(x, function(x) ArtifactRefClass$new(x, .self))
         names(x) <- sapply(x, '[[', 'name')
      }
      invisible(x)

   }) # get_artifacts



#' Get one or more samples using queries on name, projectlimsid, projectname
#' It is possible to also filter the query on UDF values but it may be easier to
#  do that after getting the samples - see \url{http://genologics.com/developer}
#' 
#' @family Lims Sample
#' @name LimsRefClass_get_samples
#' @param optional name a character vector of one or more names
#' @param optional projectlimsid character of one or more projectlimsid values
#' @param optional projectname character of one or more projectname values
#' @return a named list of SampleRefClass or NULL
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
      x <- .self$GET(.self$uri(resource), query = query, asNode = FALSE)
      if (!is_exception(x)){
         uri <- sapply(XML::xmlChildren(x), function(x) xml_atts(x)[['uri']])
         len <- sapply(uri, length)
         if (all(len == 0)) return(NULL)
         if (.self$version == "v1"){
            x <- lapply(uri, function(x, lims=NULL) {lims$GET(x)}, lims = .self)
         } else {
            x <- batch_retrieve(uri, .self, rel = 'samples')
            x <- lapply(x, function(x) SampleRefClass$new(x, .self))
         }
         names(x) <- sapply(x, '[[', 'name')
      }
      invisible(x)
   })


#' Retrieve a list of InstrumentRefClass or data.frame of the good stuff
#' @family Lims Instrument
#' @name LimsRefClass_get_instruments
#' @param optional name character a vector of one or more names
#' @param form character, return a 'data.frame' or list of Nodes
#' @return a list of  InstrumentRefClass, a data frame or NULL
NULL
LimsRefClass$methods(
   get_instruments = function(name = NULL, 
      form = c('data.frame', 'Node')[1]){
   
      resource <- 'instruments'
      
      query <- if(is.null(name)) NULL else build_query(list(name=name))
      
      RR <- .self$GET(.self$uri(resource), query = query)
      rr <- RR$node['instrument']
      if (length(rr) == 0) return(NULL)
      uri <- sapply(rr, function(x) xml_atts(x)[['uri']])
      x <- lapply(uri, function(x, lims = NULL) {
            lims$GET(x, asNode = TRUE)
         }, lims = .self)
      if (tolower(form[1]) == 'data.frame'){
         x <- data.frame (limsid = basename(uri),
            name = sapply(x, function(x) x$name),
            type = sapply(x, function(x) x$type),
            stringsAsFactors = FALSE)
      }
      invisible(x)
   }) # get_instruments

#' Retrieve a list of ResearcherRefNodes or a data.frame of the good stuff
#' 
#' @family Lims Researcher
#' @name LimsRefClass_get_researchers
#' @param optional username character a vector of one or more user names like 'btupper' etc.
#' @param form character, return a 'data.frame' or list of Nodes
#' @return a list of  ResearcherRefClass, a data frame or NULL
NULL
LimsRefClass$methods(
   get_researchers = function(username = NULL, 
      form = c('data.frame', 'Node')[1]){
   
      resource <- 'researchers'
      
      query <- if(is.null(username)) NULL else build_query(list(username=username))
      
      RR <- .self$GET(.self$uri(resource), query = query)
      rr <- RR$node['researcher']
      if (length(rr) == 0) return(NULL)
      uri <- sapply(rr, function(x) xml_atts(x)[['uri']])
      x <- lapply(uri, function(x, lims = NULL) {
            lims$GET(x, asNode = TRUE)
         }, lims = .self)
      if (tolower(form[1]) == 'data.frame'){
         x <- data.frame (limsid = basename(uri),
            name = sapply(x, function(x) x$name),
            username = sapply(x, function(x) x$username),
            initials = sapply(x, function(x) x$initials),
            email = sapply(x, function(x) x$email),
            credentials = sapply(x, function(x) x$get_credentials()),
            stringsAsFactors = FALSE)
      }
      invisible(x)
   }) # get_researchers


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
      
      resource <- 'processes'
      
      query <- list()
      if (!is.null(last_modified)) query[["last-modified"]] <- last_modified
      if (!is.null(type)) query[["type"]] <- type
      if (!is.null(inputartifactlimsid)) query[["inputartifactlimsid"]] <- inputartifactlimsid
      if (!is.null(technamefirst)) query[["technamefirst"]] <- technamefirst
      if (!is.null(technamelast)) query[["technamelast"]] <- technamelast
      if (!is.null(projectname)) query[["projectname"]] <- projectname
      if (length(query)>0) {
         query <- build_query(query)
      } else {
         query <- NULL
      }
   
      x <- .self$GET(.self$uri(resource), query = query, asNode = FALSE)
      if (length(XML::xmlChildren(x)) == 0) return(NULL)
      
      uri <- sapply(XML::xmlChildren(x), function(x) xml_atts(x)[['uri']])
      x <- lapply(uri, 
         function(x, lims = NULL) {
            lims$GET(x, asNode = TRUE)
         }, 
         lims = .self)
      
      invisible(x)
   }) # get_processes


#' Get one or more Labs - does not leverage /batch/retrieve resources
#' but provides similar behavior.
#'
#' @family Lims Labs
#' @name LimsRefClass_get_labs
#' @param name optional lab name
#' @param last_modified optional character vector in YYYY-MM-DDThh:mm:ssTZD format
#' @return a list of NodeRefClass or NULL
LimsRefClass$methods(
   get_labs = function(name = NULL, last_modified = NULL){
      
      resource <- 'labs'
      
      query <- list()
      if (!is.null(last_modified)) query[["last-modified"]] <- last_modified
      if (!is.null(name)) query[["name"]] <- name
      if (length(query)>0) query <- build_query(query)
      if (length(query) == 0) query <- NULL
      
      x <- .self$GET(.self$uri(resource), query = query, asNode = FALSE)
      if (length(XML::xmlChildren(x)) == 0) return(NULL)
      
      uri <- sapply(XML::xmlChildren(x), function(x) xml_atts(x)[['uri']])
      x <- lapply(uri, 
         function(x, lims = NULL) {
            lims$GET(x, asNode = TRUE)
         }, 
         lims = .self)
      names(x) <- sapply(x, function(x) xml_value(x$node[['name']]) )
      invisible(x)
   }) # get_labs


#' Get one or more Projects - does not leverage /batch/retrieve resources
#' but provides similar behavior.
#'
#' @family Lims Projects
#' @name LimsRefClass_get_projects
#' @param name optional project name
#' @param last_modified optional character vector in YYYY-MM-DDThh:mm:ssTZD format
#' @return a list of ProjectRefClass or NULL
LimsRefClass$methods(
   get_projects = function(name = NULL, last_modified = NULL){
      
      resource <- 'projects'
      
      query <- list()
      if (!is.null(last_modified)) query[["last-modified"]] <- last_modified
      if (!is.null(name)) query[["name"]] <- name
      if (length(query)>0) {
         query <- build_query(query)
      } else {
         query <- NULL
      }
   
      x <- .self$GET(.self$uri(resource), query = query, asNode = FALSE)
      if (length(XML::xmlChildren(x)) == 0) return(NULL)
      
      uri <- sapply(XML::xmlChildren(x), function(x) xml_atts(x)[['uri']])
      x <- lapply(uri, 
         function(x, lims = NULL) {
            lims$GET(x, asNode = TRUE)
         }, 
         lims = .self)
      
      invisible(x)
   }) # get_projects

#' Get one or more process-types as Nodes
#'
#' @family Lims Process
#' @name LimsRefClass_get_processtypes
#' @param displayname optional project name
#' @return a list of NodeRefClass or NULL
LimsRefClass$methods(
   get_processtypes = function(displayname = NULL){
      
      resource <- 'processtypes'
      
      query <- list()
      if (!is.null(displayname)) query[["displayname"]] <- displayname
      if (length(query)>0) {
         query <- build_query(query)
      } else {
         query <- NULL
      }
   
      x <- .self$GET(.self$uri(resource), query = query, asNode = FALSE)
      if (length(XML::xmlChildren(x)) == 0) return(NULL)
      
      uri <- sapply(XML::xmlChildren(x), function(x) xml_atts(x)[['uri']])
      x <- lapply(uri, 
         function(x, lims = NULL) {
            lims$GET(x, asNode = TRUE)
         }, 
         lims = .self)
      names(x) <- sapply(x, '[[','name' )
      invisible(x)
   }) # get_processtypes


#' Get one or more fields (UDF) as Nodes
#' 
#' @family Lims Field
#' @name LimsRefClass_get_fields
#' @param name one or more optional UDF names
#' @param attach_to_name one name of a container, sample, project, process
#' @param attach-to-category If 'attach_to_name' is the name of a process, 
#'  specify 'ProcessType'. Must not be provided otherwise. 
#' @param ... further arguments for GET method
#' @return a list of FieldRefClass or NULL
NULL
LimsRefClass$methods(
    get_fields = function(
        name = NULL, 
        attach_to_name = NULL, 
        attach_to_category = NULL,
        ...){
       
        resource <- 'configuration/udfs'
        
        queryl <- list()
        if (!is.null(name)) queryl[["name"]] <- name
        if (!is.null(attach_to_name)) queryl[["attach-to-name"]] <- attach_to_name
        if (!is.null(attach_to_category)) 
            queryl[["attach-to-category"]] <- attach_to_category
        query <- build_query(queryl)
        x <- .self$GET(.self$uri(resource), query = query, asNode = FALSE,...)
        if (length(XML::xmlChildren(x)) == 0) return(NULL)
        
        uri <- sapply(XML::xmlChildren(x), function(x) xml_atts(x)[['uri']])
        x <- lapply(uri, 
           function(x, lims = NULL) {
              lims$GET(x, asNode = TRUE)
           }, 
           lims = .self)
        names(x) <- sapply(x, '[[','name')
        invisible(x)
    }) # get_fields
    


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
         stop("LimsRefClass$batchretrieve rel must be one of artifacts, files, samples or containers")
      if ((.self$version == "v1") && (rel %in% c('samples', 'files')) ){
         x <- lapply(uri, function(x, lims=NULL) {lims$GET(x, asNode = FALSE)}, lims = .self)
      } else {
         x <- batch_retrieve(uri, .self, rel = rel[1], rm_dups = rm_dups, ...)   
      }
      #x <- batch_retrieve(uri, .self, rel = rel[1], rm_dups = rm_dups, ...)   
      if (asNode) {
         x <- lapply(x, parse_node, .self)
         names(x) <- switch(rel[1],
            'files' = names(x),
            sapply(x, function(x) xml_value(x$node[['name']]))  )
      }
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
         x <- lapply(x, function(x) x$node)
      }
      ok <- sapply(x, is_xmlNode)
      if (!all(ok)) 
         stop("LimsRefClass$batchupdate: inputs must inherit xmlNode or NodeRefClass")
      nm <- unique(sapply(x, xml_name))
      if (length(nm) > 1) 
         stop("LimsRefClass$batchupdate: all nodes must be of the same type - ", paste(nm, collapse = " "))
      if (!(nm %in% c("artifact", "sample", "container")))
         stop("LimsRefClass$batchupdate: only artifact, sample and container types have batch update")
      
      detail <- switch(nm,
         'artifact' = create_artifacts_details(x),
         'container' = create_containers_details(x),
         'sample' = create_samples_details(x),
         NULL)
      
      if (is.null(detail)) stop("LimsRefClass$batchupdate: only artifact, sample and container types have batch update")
      
      URI <- .self$uri(paste0(nm, "s/batch/update"))
      r <- httr::POST(URI, ..., body = xmlString(detail), 
         httr::add_headers(c("Content-Type"="application/xml")),
         handle = .self$handle,
         .self$auth)
      
      x <- .self$check(r)
      if (!is_exception(x)) {
         rel <- paste0(nm, "s")
         uri <- sapply(x['link'], function(x) xml_atts(x)[['uri']])
         r <- batch_retrieve(uri, .self, , rel = rel, ...)
         if (asNode) {
            r <- lapply(r, parse_node, .self)
            # since we have sample, artifact or container we know we can have 
            # a name
            names(r) <- sapply(r, function(x) x$name)
         }
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
         stop("LimsRefClass$batchcreate: inputs must inherit xmlNode or NodeRefClass")
      nm <- unique(sapply(x, xml_name))
      if (length(nm) > 1) 
         stop("LimsRefClass$batchcreate: all nodes must be of the same type - ", paste(nm, collapse = " "))
      if (!(nm %in% c("samplecreation", "container")))
         stop("LimsRefClass$batchcreate: only samplecreation and container types have batch create")
      
      detail <- switch(nm,
         'container' = create_containers_details(x),
         'samplecreation' = create_samples_details(x),
         NULL)
         
      # the name of the nodes may be different than the resource namespace
      resource <- switch(nm,
         'samplecreation' = 'sample',
         nm)
         
      if (is.null(detail)) stop("LimsRefClass$batchcreate: only samplecreation and container types have batch create")
      
      URI <- .self$uri(paste0(resource, "s/batch/create"))
      r <- httr::POST(url=URI, body = xmlString(detail), 
         #httr::add_headers(c("Content-Type"="application/xml")),
         httr::content_type_xml(),
         #handle = .self$handle,
         .self$auth)
      
      x <- .self$check(r)
      if (!is_exception(x)) {
         rel <- paste0(resource, "s")
         uri <- sapply(x['link'], function(x) xml_atts(x)[['uri']])
         r <- batch_retrieve(uri, .self, , rel = rel)
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
      httr::add_headers(c("Content-Type"="application/xml")),
      handle = lims$handle,
      lims$auth)
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
         
      nmspc <- unclass(XML::xmlNamespaces(x, simplify = TRUE))
      xx <- x[singleName]
      # transfer the the xmlnamespace to each child node
      # uristub = get_NSMAP()[[nm]]
      xx <- lapply(xx, 
         function(x, nm=NULL) {
                  for (n in names(nm)) dummy <- XML::newXMLNamespace(x,nm[n])
                  x}, 
            nm = nmspc)
      if (rm_dups == FALSE) {
         # name each node
         names(xx) <- sapply(xx, function(x) xml_atts(x)["limsid"])
         # rebuild the list with duplicates
         xx <- xx[limsid_all]
      }
   } else {
      xx <- x
   }
   invisible(xx) 
} # batch_retrieve


#' Get a uri with option to depaginate
#'
#' @export
#' @param uri character, the uri to retrieve
#' @param lims a LimsRefClass node
#' @param ... further arguments for httr::GET()
#' @param depaginate logical, if TRUE (the default) then depaginate the results
#' @param verbose logical if TRUE be verbose
#' @return XML::xmlNode
get_uri <- function(uri, lims, ..., depaginate = TRUE, verbose = FALSE){


      if (verbose) cat("get_uri:", uri, "\n")

      # since when has LIMS substituted "+" for spaces ("%20")?
      # @param uri
      # @return updated param
      no_plus_uri <- function(x){
         file.path(dirname(x), gsub("+", "%20", basename(x), fixed = TRUE))
      }
      
      uri <- no_plus_uri(uri)
      # first pass
      x <- httr::GET(uri,  
         ...,
         encoding = lims$encoding,
         handle = lims$handle,
         lims$auth)


      x <- lims$check(x) 
      if ( !is_exception(x) && ("next-page" %in% names(x))  && depaginate ){
         yuri <- no_plus_uri(xml_atts(x[['next-page']])[['uri']])
         doNext <- TRUE
         while(doNext){
            y <- lims$check(httr::GET(yuri, encoding = lims$encoding,
               handle = lims$handle, lims$auth))
            children <- !(names(y) %in% c("previous-page", "next-page"))
            if (any(children)) x <- XML::addChildren(x, kids = y[children])
            doNext <- "next-page" %in% names(y)
            if (doNext) yuri <- no_plus_uri(xml_atts(y[["next-page"]])[["uri"]])
         } # doNext while loop
         x <- XML::removeChildren(x, kids = x["next-page"])
      }
   invisible(x)
}


#' Convert a node to an object inheriting from NodeRefClass 
#'
#' @family Lims Node
#' @export
#' @param node XML::xmlNode
#' @param lims LimsRefClass object
#' @return an object the inherits from NodeRefClass
parse_node <- function(node, lims){

   if (!is_xmlNode(node)) stop("parse_node: node must be XML::xmlNode")
   if (!inherits(lims, 'LimsRefClass')) stop("assign_node: lims must be LimsRefClass")
   
   nm <- xml_name(node)[1]
   switch(nm,
       'artifact' = ArtifactRefClass$new(node, lims),
       'processes' = ProcessRefClass$new(node, lims),
       'process' = ProcessRefClass$new(node, lims),
       'container' = ContainerRefClass$new(node, lims),
       'sample' = SampleRefClass$new(node,lims),
       'input-output-map' = InputOutputMapRefClass$new(node, lims),
       'researcher' = ResearcherRefClass$new(node, lims),
       'file' = FileRefClass$new(node, lims),
       'field' = FieldRefClass$new(node, lims),
       'project' = ProjectRefClass$new(node, lims),
       'projects' = ProjectsRefClass$new(node, lims),
       'container-type' = ContainerTypeRefClass$new(node, lims),
       'instrument' = InstrumentRefClass$new(node, lims),
       'process-type' = ProcessTypeRefClass$new(node, lims),
       'exception' = ExceptionRefClass$new(node, lims),
       'artifactgroup' = ArtifactGroupRefClass$new(node, lims),
       NodeRefClass$new(node, lims))

}


#' Instantiate a LimsRefClass object
#'
#' @export
#' @param configfile character, the fully qualified path to the config file
#' @return a LimsRefClass instance or NULL
Lims <- function(configfile = build_config_path()){
   if (!file.exists(configfile[1])) stop("configfile not found:", configfile[1])
   x <- try(read_config(configfile[1]))
   if (inherits(x, "try-error")) stop("Error reading config file")
   
   X <- LimsRefClass$new()
   X$field("encoding", "UTF-8")
   X$field("version", get_config(x, "genologics", "VERSION", default = ""))
   buri <- get_config(x, "genologics", "BASEURI", default = "")
   if (nchar(buri) == 0) stop("base uri not found in config file")
   X$field("baseuri", file.path(buri, "api", X$version))
   X$field("handle", httr::handle(buri))
   X$field('auth', 
      httr::authenticate(get_config(x, "genologics", "USERNAME", default = ""),
                   get_config(x, "genologics", "PASSWORD", default = "") 
      ) )
   X$field('fileauth',
      httr::authenticate(get_config(x, "glsfilestore", "USERNAME", default = ""),
                   get_config(x, "glsfilestore", "PASSWORD", default = "") 
      ) )   
   if (!X$validate_session()) {
      warning("API session failed validation")
   } 
   X
}
