# misc.R


#' Prepares a contribution to a query list - see \code{modify_url}
#'
#' @export
#' @param x a named list like \code{list(name=c("boo", "foo"), type = "shoe")}
#' @return character like "name=boo&name=foo$type=shoe"
build_query <- function(x){
   if (!is.list(x)) stop("build_query: x must be a list")
   nm <- names(x)
   if (length(nm) == 0) stop("build_query: x must be a named list")
   # create a character vector, iterate along the input transforming each element
   # to the name1=value&name2=value
   # then join the whole shooting match
   s <- vector(mode = 'character', length = length(x))
   for (i in seq_along(x)){
      n <- nm[i]
      s[i] <- paste(paste(gsub("_","-", n), curl::curl_escape(x[[n]]), sep = "="), collapse = "&")
   }
   paste(s, collapse = "&")
}
   
#' Return a vector of unique names from a objeect.  The object must have the
#' methods for \code{names}
#'
#' @export
#' @param x the object
#' @return a vector of unqiue names or NULL
unames <- function(x){
   unique(names(x))
} 


#' Test if an object inherits from XML::XMLAbstractNode
#'
#' @export
#' @param x object to test
#' @param classname character, the class name to test against, by default 'XMLAbstractNode'
#' @return logical
is_xmlNode <- function(x, classname = 'XMLAbstractNode'){
   inherits(x, classname)
}

#' Convert XML::xmlNode to character
#' 
#' @export
#' @param x xmlNode
#' @return character
xmlString <- function(x){
   gsub("\n","", XML::toString.XMLNode(x))
}


#' Test XML::xmlNode is an exception
#'
#' @export
#' @param node object to test
#' @param space the namespace to test
#' @return logical
is_exception <- function(x, space = 'exc'){
   is_xmlNode(x) && ("exc" %in% names(XML::xmlNamespace(x)) )
}



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

#' Split a uri around the "?" or other character.  For example, 
#' \code{trimuri(http://www.uri.org?where=foo)} returns \code{http://www.uri.org}.
#' 
#' @export
#' @param uri character, the uri to split
#' @param around character,the character to split around
#' @param fixed see \code{\link{strsplit}}
#' @param index numeric, by default only the leading portion of the input is
#' returned
#' @return character of the input preceding the 'around' character
trimuri <- function(uri, around = "?", fixed = TRUE, index = 1){
   if (!is.character(uri)) {stop("URI must be character")}
   sapply(strsplit(uri, around, fixed = fixed), '[', index)
}




#' Retrieve a named vector of UDF types to R types
#' @export
#' @family Node
#' @param x character of the UDF type
#' @return named character vector if equivalent R types
udf_type2R <- function(x){
   lut <- c(
      'String' = 'character',
      'Text' = 'character',
      'Boolean' = 'logical',
      'Numeric' = 'numeric',
      'Date' = 'character',
      'URI' = 'character')
   if (!missing(x)){
      return(lut[x])
   } else {
      return(lut)
   }
}
      
#' Retrieve the Genologics namespace map
#' @export
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
