# misc.R

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

#' Split a uri around the "?" or other character
#' 
#' @export
#' @param uri character, the uri to split
#' @param around character,the character to split around
#' @param fixed see \code{\link{strsplit}}
#' @param index numeric, by default only the leading portion of the input is
#' returned
#' @return character of the input preceding the 'around' character
splituri <- function(uri, around = "?", fixed = TRUE, index = 1){
   if (!is.character(uri)) {stop("URI must be character")}
   sapply(strsplit(uri, around, fixed = fixed), '[', index)
}


#' Create an unresolved file node
#' 
#' @export
#' @family Lims
#' @param attached_to character uri of the artifact to attach the file to
#' @param original_location character, the fully qualified path of the original file
#' @param namespace character the namespace for the resource
#' @return XML::xmlNode
create_file_node <- function(attached_to = "", original_location = "",
   namespace = 'file'){
   nsr <- get_NSMAP()[namespace[1]]
   newXMLNode(namespace[1],
      namespace = namespace[1],
      namespaceDefinitions = nsr,
      .children = list(
         newXMLNode("attached-to", attached_to),
         newXMLNode("original-location", original_location)) )
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
