   
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
      x <- httr::GET(.self$baseuri, handle = .self$handle, .self$auth)
      if (httr::status_code(x) != 200) {
         warning("response has non-200 status code")
         print(x)
         ok <- FALSE
      }
      ok
   })


#' GET a resource
#'
#' @name LimsRefClass_GET
#' @param uri the uri to get
#' @return XML node - possible an error node or NULL
NULL
LimsRefClass$methods(
   GET = function(uri){
      r <- httr::GET(uri, handle = handle, authenticate(.self$username, .self$password))
      if ( (class(r) != 'response') ){
         warning("error in response")
         print(r)
         return(NULL)
      }
      if (status_code(r) != 200){
         warning("response code not 200")
         print(r)
         return(NULL)
      }   
      x <- xmlRoot(content(r, type = "text/xml"))
      if (inherits(x, "try-error")){
         x <- .self$create_exception(message = "error parsing content with xmlRoot")
      }
      invisible(x)  
   }) #GET


#' Create an exception node
#' @param message chracter, some error message
#' @return xmlNode
LimsRefClass$methods(
   create_exception = function(message = 'Unspecified exception'){
      x <- newXMLNode("exception", 
         namespaceDefinitions = .self$NSMAP[['exc']], 
         namespace = 'exc')
      x <- addChildren(x, kids = list(newXMLNode("message", message)) )
      x
   }) #create_exception
#### methods above
#### functions below

#' Read a configuration file
#' @param filename the name of the file
#' @return a name list with one element per section
#'  each section, in turn is a named list
read_config = function(filename){
   #' Parse lines of text
   #' @param x the line(s) of text in the form of tag value pairs
   #' @return named character vector of tag1 = value1, tag2 = value2, ...
   parse_config_line <- function(x){
    pat <- '[=:]'
    ix <- regexpr(pat, x)
    r <- vector(mode = "character")
    for (i in seq_along(x)){
      if (ix[i] > 0 ){
        nm <- substring(x[i], 1, ix[i]-1)
        # strip leading spaces
        val <- sub("^ +", "", substring(x[i], ix[i] + 1, nchar(x[i]) ) )
        r[[nm]] <- val
      } # ix[i] > 0
    } # i-loop
    r
  }
 
   if (!file.exists(filename)) stop(paste("file must exist: ", filename))
   x <- scan(filename, what = character(), quiet = TRUE, sep = "\n", comment.char = "#")
   x <- x[!grepl("^#", x)]
   ix <- grep("^\\[", x)
   if (length(ix) == 0) stop("No [headers] found in config file - please check file")
   len <- nchar(x[ix])
   nm <- substring(x[ix], rep(2, length(len)), len-1)
   iy <- c(ix, length(x)+1)
   L <- list()
   for (i in seq_along(nm)) L[[nm[i]]] <- parse_config_line(x[(iy[i] + 1) : (iy[i+1]-1) ])
   invisible(L)
} # read_config

#' Retrieve a configuration value
#' @param x the configuration list
#' @param section the name of the section
#' @param name the name of the tagged value, if missing then the section is returned
#' @param default the value to return if the tag doesn't exists (defaults to NULL)
#' @return the tagged value or section requested or the 'default' value if not found
get_config <- function(x, section, name, default = NULL){
   if (nargs() < 2) stop("at least x and section are required")
   s <- x[[section[1]]]
   if (is.null(s)) return(default)
   if ( !(missing(name)) ) {
     if (name[1] %in% names(s)) return(s[[name[1]]])
   } else {
     return(s)
   }
   return(default)
} # get_config


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

#' Namespace map
#' @family Lims
#' @return a named vector of xmlNameSpaces
get_NSMAP <- function(){ 
   c(
    art='http://genologics.com/ri/artifact',
    artgr='http://genologics.com/ri/artifactgroup',
    cnf='http://genologics.com/ri/configuration',
    con='http://genologics.com/ri/container',
    ctp='http://genologics.com/ri/containertype',
    exc='http://genologics.com/ri/exception',
    file='http://genologics.com/ri/file',
    inst='http://genologics.com/ri/instrument',
    lab='http://genologics.com/ri/lab',
    prc='http://genologics.com/ri/process',
    prj='http://genologics.com/ri/project',
    prop='http://genologics.com/ri/property',
    protcnf='http://genologics.com/ri/protocolconfiguration',
    protstepcnf='http://genologics.com/ri/stepconfiguration',
    prx='http://genologics.com/ri/processexecution',
    ptm='http://genologics.com/ri/processtemplate',
    ptp='http://genologics.com/ri/processtype',
    res='http://genologics.com/ri/researcher',
    ri='http://genologics.com/ri',
    rt='http://genologics.com/ri/routing',
    rtp='http://genologics.com/ri/reagenttype',
    smp='http://genologics.com/ri/sample',
    stg='http://genologics.com/ri/stage',
    stp='http://genologics.com/ri/step',
    udf='http://genologics.com/ri/userdefined',
    ver='http://genologics.com/ri/version',
    wkfcnf='http://genologics.com/ri/workflowconfiguration')
}   # get_NSMAP