# Process.R

#' An Process representation that sublcasses from NodeRefClass
#' 
#' @family Node
#' @include Node.R
#' @export
ProcessRefClass <- setRefClass("ProcessRefClass",
   contains = "NodeRefClass")

Process <- getRefClass("ProcessRefClass")

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
         x <- xmlValue(dr)
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
#' @param form 'First Last', or 'uri'
#' @return character or "" if not available
NULL
ProcessRefClass$methods(
   technician = function(form = c("First Last", "uri")[1]){
      technode <- .self$node[['technician']]
      if (is.null(technode)){
         x <- ""
      } else {
         x <- switch(form,
            'uri' = xmlAttrs(technode)[['uri']],
            paste(xmlValue(technode[['first-name']]),
               xmlValue(technode[['last-name']]) ) )
      }
      x
   }) #technician
   
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
            'uri' = xmlAttrs(typenode)[['uri']],
            xmlValue(typenode) )
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
      thisuri <- trimuri(xmlAttrs(.self$node[["parent-process"]])[['uri']])
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
         x <- data.frame(
               input_limsid = inputlimsid,
               input_uri = sapply(x, "[[", "input_uri"),
               post_process_uri = sapply(x, "[[", "post_process_uri"),
               output_limsid = sapply(x, "[[", "output_limsid"),
               output_uri = sapply(x, "[[", "output_uri"),
               output_generation_type = sapply(x, "[[", "output_generation_type"),
               output_type = sapply(x, "[[", "output_type"),
               stringsAsFactors = FALSE, 
               rownames = inputlimsid)
      }
      invisible(x)
   })
   


#' Get either the input or the output artifacts (or both!) as NodeRefClass or uri
#' 
#' @family Process
#' @name ProcessRefClass_get_artifacts
#' @param what either 'input', 'output' or 'both'
#' @param form character of either 'Node' or 'uri'
#' @param a list of ArtifactRefClass objects or uri unless \code{what} is both in which case
#'    a list is returned with 'input' and 'output' elements
NULL
ProcessRefClass$methods(
   get_artifacts = function(what = c("input", "output", "both")[1],
      form = c("Node", "uri")[1]){
      
      iom <- .self$get_inputoutputmap(asDataFrame = TRUE)
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
   
   x <- newXMLNode("process",
      newXMLNode("type", type),
      newXMLNode("date-run", dateRun),
      newXMLNode("technician", attrs = c(uri=technician)),
      ...,
      namespaceDefinitions = get_NSMAP()[c("udf", "prx", "inst")], 
      namespace = "prx") 
   if (!is.null(instrument))  x <- addChildren(x,
         kids = list(newXMLNode("instrument", 
            attrs = list(uri=instrument))) )
   x
}
