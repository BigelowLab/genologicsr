# Process.R

ProcessRefClass <- setRefClass("ProcessRefClass",
   contains = "NodeRefClass")

#' Show
#' 
#' @family Node Process
NULL
ProcessRefClass$methods(
   show = function(prefix = ""){
      callSuper(prefix = prefix)
      cat(prefix, "  date-run: ", .self$date_run(), "\n", sep = "")
      cat(prefix, "  technician: ", .self$technician(), "\n", sep = "")
      cat(prefix, "  type: ", .self$type(), "\n", sep = "")
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
   
#' Retrieve the process type
#'
#' @param form 'name', or 'uri'
#' @return character or "" if not available
NULL
ProcessRefClass$methods(
   type = function(form = c("name", "uri")[1]){
      typenode <- .self$node[['type']]
      if (!is.null(typenode)) {
         x <- switch(form,
            'uri' = xmlAttrs(typenode)[['uri']],
            xmlValue(typenode) )
      } else {
         x <- ""
      }
      x
   }) #type
   
Process <- getRefClass("ProcessRefClass")