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
      if (!is.null(typenode)) {
         x <- XML::xmlAttrs(typenode)[['uri']]
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
#' @param asDataFrame logical, if TRUE then return a data.frame
#' @return a list of InputOutputMapRefClass or a data.frame or NULL if not available
NULL
ProcessRefClass$methods(
   get_inputoutputmap = function(asDataFrame = TRUE){
      
      if (!is_xmlNode(.self$node) || !.self$has_child('input-output-map')) {
         return(NULL)
      }      
      x <- lapply(.self$node['input-output-map'], function(x, lims = NULL){
            InputOutputMap$new(x, lims)
         }, lims = .self$lims)
         
      if (asDataFrame == TRUE){
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
#'   \code{get_inputoutputmap(asDataFrame = TRUE)}.  If not provided (or NULL)
#'   then this method will fetch it.
#' @return a list of ArtifactRefClass objects or uri unless \code{what} is 
#'    both in which case a list is returned with 'input' and 'output' elements
NULL
ProcessRefClass$methods(
   get_artifacts = function(
      what = c("input", "output", "both")[1],
      form = c("Node", "uri")[1], 
      iom = NULL){
      
      if (is.null(iom)) iom <- .self$get_inputoutputmap(asDataFrame = TRUE)
      what <- tolower(what[1])
      form <- tolower(form[1])
      
      if (what == "input"){
      
         x <- iom[,'input_uri']
         if (form == "node"){
            x <- .self$lims$batchretrieve(x, rel = 'artifacts')
         }
         
      } else if (what == "output"){
      
         x <- iom[,'output_uri']
         if (form == "node"){
            x <- .self$lims$batchretrieve(x, rel = 'artifacts')
         }
         
      } else {  # both!
      
         input <- iom[,'input_uri']
         output <- iom[,'output_uri']
         if (form == "node"){
            input <- .self$lims$batchretrieve(input, rel = 'artifacts', rm_dups = FALSE)
            output <- .self$lims$batchretrieve(output, rel = 'artifacts', rm_dups = FALSE)
         }
         x <- list(input = input, output = output)      
      }
         
      invisible(x)
   }) # get_artifacts


#' Retrieve file artifacts (either input, output or both) - very similar to 
#' get_artifacts but looks for Artifacts types with the keyword 'file' - 'ResultFile',
#' 'SharedResultFile', etc.  Requestion 'output' is fairly quick as the 
#' input-output-map carris the output-type attribute. Requesting 'input' requires 
#' more time as each input artifact must be fetched before determining it's type.
#' 
#' @family Process
#' @name ProcessRefClass_get_file_artifacts
#' @param what character of 'input', 'output' or 'both'
#' @param form character of either 'Node' or 'uri'
#' @param iom an optional data frame of input-output-map data as per 
#'   \code{get_inputoutputmap(asDataFrame = TRUE)}.  If not provided (or NULL)
#'   then this method will fetch it.
#' @return a two element list of ArtifactRefClass objects or uri unless \code{what}
#' with zero or more elements of 'input' and zero or more elements of 'output'
NULL
ProcessRefClass$methods(
   get_file_artifacts = function(
      what = c("input", "output", "both")[1],
      form = c("Node", "uri")[1], 
      iom = NULL){
      
      if (is.null(iom)) iom <- .self$get_inputoutputmap(asDataFrame = TRUE) 
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
            AA <- .self$lims$batchretrieve(iom[ix,'output_uri'], rel = 'artifacts')
            OUT <- sapply(AA, function(x) XML::xmlAttrs(x$node[['file']])[['uri']] )
            if (form == "node") OUT <- lapply(OUT, function(x) .self$lims$GET(x) )
         } # any files? 
      }
      
      X <- list()
      if (!is.null(IN)) X[['input']] <- IN
      if (!is.null(OUT))  X[['output']] <- OUT
      
      if (length(X) == 0){
         return(NULL)
      } else {
         return(X)
      }
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
