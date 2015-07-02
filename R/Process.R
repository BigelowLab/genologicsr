# Process.R

ProcessRefClass <- setRefClass("ProcessRefClass",
   contains = "NodeRefClass")

Process <- getRefClass("ProcessRefClass")

#' Show
#' 
#' @family Node Process
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
ProcessRefClass$methods(
   get_inputoutputmap = function(asDataFrame = TRUE){
      if (!is_xmlNode(.self$node) || !.self$has_child("input-output-map')) {
         return(NULL)
      }      
      x <- lapply(.self$node['input-output-map'], function(x, lims = NULL){
            InputOutputMap$new(x, lims)
         }, lims = .self$lims)
      if (asDataFrame){
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
   
   