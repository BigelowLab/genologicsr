# Process.R

#' An Process representation that sublcasses from NodeRefClass
#' 
#' @family Node
#' @include Node.R
#' @export
ProcessRefClass <- setRefClass("ProcessRefClass",
   contains = "NodeRefClass",
   methods = list(
      initialize = function(...){
         callSuper(...)
         .self$verbs <- c("GET", "PUT", "BROWSE")
      })
   )
   
#' Show
#' 
#' @family Node Process
#' @name ProcessRefClass_show
NULL
ProcessRefClass$methods(
   show = function(prefix = ""){
      callSuper(prefix = prefix)
      cat(prefix, "  Process date-run: ", .self$date_run(), "\n", sep = "")
      cat(prefix, "  Process technician: ", .self$technician(), "\n", sep = "")
      cat(prefix, "  Process type: ", .self$get_type(), "\n", sep = "")
   })
   
#' Retrieve the date run as string or POSIXct
#'
#' @family Process
#' @name ProcessRefClass_date_run
#' @param as_POSIX logical, by default return a string
#' @return character, POSIXct or "" if not available
NULL
ProcessRefClass$methods(
   date_run = function(as_POSIX = FALSE){
      dr <- .self$node[['date-run']]
      if (!is.null(dr)) {
         x <- XML::xmlValue(dr)
         if (as_POSIX) x <- as.POSIXct(x)
      } else {
         x <- ""
      }
      x
   }) #date_run
   
#' Retrieve the technican "First Last" name
#' 
#' @family Process
#' @name ProcessRefClass_technician
#' @param form 'First Last', "Node" or 'uri'
#' @return ResearcherRefClass, character or NULL if not available
NULL
ProcessRefClass$methods(
   technician = function(form = c("First Last", "Node", "uri")[1]){
      technode <- .self$node[['technician']]
      if (is.null(technode)){
         x <- NULL
      } else {
         x <- switch(tolower(form[1]),
            'uri' = XML::xmlAttrs(technode)[['uri']],
            'node' = .self$lims$GET(XML::xmlAttrs(technode)[['uri']]), 
            paste(XML::xmlValue(technode[['first-name']]),
               XML::xmlValue(technode[['last-name']]) ) )
      }
      x
   }) #technician

#' Retrieve instrument information (if any)
#'
#' @family Process
#' @name ProcessRefClass_instrument
#' @param form 'Node', or 'uri'
#' @return InstrumentRefClass, character or NULL if not available
NULL
ProcessRefClass$methods(
   instrument = function(form = c("Node", "uri")[1]){
      inode <- .self$node[['instrument']]
      if (!is.null(inode)) {
         x <- XML::xmlAttrs(inode)[['uri']]
         if (tolower(form == 'node')) x <- .self$lims$GET(x)
      } else {
         x <- NULL
      }
      x
   }) #instrument
     
#' Retrieve the process type, overrides NodeRefClass_get_type
#'
#' @family Process
#' @name ProcessRefClass_get_type
#' @param form 'name', or 'uri'
#' @return character or "" if not available
NULL
ProcessRefClass$methods(
   get_type = function(form = c("name", "uri")[1]){
      typenode <- .self$node[['type']]
      if (!is.null(typenode)) {
         x <- switch(form,
            'uri' = XML::xmlAttrs(typenode)[['uri']],
            XML::xmlValue(typenode) )
      } else {
         x <- ""
      }
      x
   }) #get_type
   
#' Get the parent_process assocated with this process
#'
#' @family Process
#' @name ProcessRefClass_get_parent_process
#' @param form character flag of type to return "Node" or "uri"
#' @return XML::xmlNode (or NULL) or the uri (or "")
NULL
ProcessRefClass$methods(
   get_parent_process = function(form = c("Node", "uri")[2]){
      if (!.self$has_child("parent-process")){
         x <- switch(tolower(form),
            "uri" = "",
            NULL)
         return(x)
      }
      thisuri <- trimuri(XML::xmlAttrs(.self$node[["parent-process"]])[['uri']])
      if (tolower(form == "uri")){
         x <- thisuri
      } else {
         x <- .self$lims$GET(thisuri, asNode = TRUE)
      }
      invisible(x)
   })  

#' Get the input-output-maps as InputOutputMapRefClass or a data.frame
#'
#' @family Process InputOutputMap
#' @name ProcessRefClass_get_inputoutputmap
#' @param form character return 'data.frame' or 'Node'
#' @return a list of InputOutputMapRefClass or a data.frame or NULL if not available
NULL
ProcessRefClass$methods(
   get_inputoutputmap = function(form = c("data.frame", "Node")[1]){
      
      if (!is_xmlNode(.self$node) || !.self$has_child('input-output-map')) {
         return(NULL)
      }      
      x <- lapply(.self$node['input-output-map'], function(x, lims = NULL){
            InputOutputMap$new(x, lims)
         }, lims = .self$lims)
         
      if (tolower(form[1]) == 'data.frame'){
         inputlimsid <- sapply(x, "[[", "input_limsid")
         output_limsid = sapply(x, "[[", "output_limsid")
         x <- data.frame(
               input_limsid = inputlimsid,
               input_uri = sapply(x, "[[", "input_uri"),
               post_process_uri = sapply(x, "[[", "post_process_uri"),
               output_limsid = output_limsid,
               output_uri = sapply(x, "[[", "output_uri"),
               output_generation_type = sapply(x, "[[", "output_generation_type"),
               output_type = sapply(x, "[[", "output_type"),
               stringsAsFactors = FALSE)
      }
      invisible(x)
   })
   


#' Get either the input or the output artifacts (or both!) as NodeRefClass or uri
#' 
#' @family Process
#' @name ProcessRefClass_get_artifacts
#' @param what either 'input', 'output' or 'both'
#' @param form character of either 'Node' or 'uri'
#' @param iom an optional data frame of input-output-map data as per 
#'   \code{get_inputoutputmap(form = 'data.frame')}.  If not provided (or NULL)
#'   then this method will fetch it.
#' @return two element list of 'input' and 'output' ArtifactRefClass objects (lists) 
#'  or uri (charcater vectors)
NULL
ProcessRefClass$methods(
   get_artifacts = function(
      what = c("input", "output", "both")[1],
      form = c("Node", "uri")[1], 
      iom = NULL){
      
      if (is.null(iom)) iom <- .self$get_inputoutputmap(form = 'data.frame')
      what <- tolower(what[1])
      form <- tolower(form[1])
      
      
      R <- list(input = NULL, output = NULL)
      if (what == "input"){
      
         R[['input']] <- iom[,'input_uri']
         if (form == "node"){
            R[['input']] <- .self$lims$batchretrieve(R[['input']], rel = 'artifacts')
         }
         
      } else if (what == "output"){
      
         R[['output']] <- iom[,'output_uri']
         if (form == "node"){
            R[['output']] <- .self$lims$batchretrieve(R[['output']], rel = 'artifacts')
         }
         
      } else {  # both!
      
         R[['input']] <- iom[,'input_uri']
         R[['output']] <- iom[,'output_uri']
         if (form == "node"){
            R[['input']] <- .self$lims$batchretrieve(R[['input']], rel = 'artifacts', rm_dups = FALSE)
            R[['output']] <- .self$lims$batchretrieve(R[['output']], rel = 'artifacts', rm_dups = FALSE)
        }
     }
         
    invisible(R)
   }) # get_artifacts


#' Retrieve file artifacts (either input, output or both) - very similar to 
#' get_artifacts but looks for Artifacts types with the keyword 'file' - 'ResultFile',
#' 'SharedResultFile', etc.  Requesting 'output' is fairly quick as the 
#' input-output-map carries the output-type attribute. Requesting 'input' requires 
#' more time as each input artifact must be fetched before determining it's type.
#' 
#' @family Process
#' @name ProcessRefClass_get_file_artifacts
#' @param what character of 'input', 'output' or 'both'
#' @param form character of either 'Node' or 'uri'
#' @param iom an optional data frame of input-output-map data as per 
#'   \code{get_inputoutputmap(form = 'data.frame')}.  If not provided (or NULL)
#'   then this method will fetch it.
#' @return a two element list ('input', 'output') of ArtifactRefClass objects or uri 
#'  depending upon the value of \code{what} one list may be empty
NULL
ProcessRefClass$methods(
   get_file_artifacts = function(
      what = c("input", "output", "both")[1],
      form = c("Node", "uri")[1], 
      iom = NULL){
      
      if (is.null(iom)) iom <- .self$get_inputoutputmap(form = 'data.frame') 
      what <- tolower(what[1])
      form <- tolower(form[1])
      
      IN <- NULL
      if (what %in% c("both", "input")){
         input_uri <- unique(iom[,'input_uri'])
         AA <- .self$lims$batchretrieve(input_uri, rel = 'artifacts')
         ix <- grepl('file', tolower(sapply(AA,function(x) x$type)), fixed = TRUE)
         if (any(ix)){
            IN <- sapply(AA, function(x) XML::xmlAttrs(x$node[['file']])[['uri']] )
            if (form == "node") IN <- lapply(IN, function(x) .self$lims$GET(x) )  
         } # any files?
      }
      
      OUT <- NULL
      if (what %in% c("both", "output")){
         ix <- grepl('file', tolower(iom[,'output_type']), fixed = TRUE)
         if (any(ix)){
            OUT <- .self$lims$batchretrieve(iom[ix,'output_uri'], rel = 'artifacts')
            if (form == "uri") OUT <- sapply(OUT, function(x) x$uri )
         } # any files? 
      }
      
      X <- list(input = IN, output = OUT)
      
    return(x)
   }) # get_file_artifacts
   
   
   

######## methods above
######## functions below

#' Create a process node used for process creation
#'
#' @family Process
#' @export
#' @param type character, the name of the process ("FACS-2", ...)
#' @param technician character,the uri of the technician
#' @param dateRun character, the date of the run
#' @param instrument character, optional, the URI of the instrument
#' @param xmlns named character vector, see \code{\link{newXMLNode}}
#' @param ... further arguments for \code{\link{newXMLNode}} including child nodes
#' @return xmlNode of type 'process'
create_process_node <- function(type = "processTypeName",
      technician = "researcherURI",
      dateRun = format(Sys.Date(), "%Y-%m-%d"),
      instrument = NULL, ... ){
   
   x <- XML::newXMLNode("process",
      XML::newXMLNode("type", type),
      XML::newXMLNode("date-run", dateRun),
      XML::newXMLNode("technician", attrs = c(uri=technician)),
      ...,
      namespaceDefinitions = get_NSMAP()[c("udf", "prx", "inst")], 
      namespace = "prx") 
   if (!is.null(instrument))  x <- XML::addChildren(x,
         kids = list(XML::newXMLNode("instrument", 
            attrs = list(uri=instrument))) )
   x
}
