# Sample.R

SampleRefClass <- setRefClass("SampleRefClass",
   contains = "NodeRefClass",
   fields = list(
      name = 'character',
      type = 'character',
      date_received = 'character',
      date_completed = 'character',
      biosource = 'character'
      ),
   methods = list(
      initialize = function(...){
         callSuper(...)
         .self$name = xmlValue(.self$node[['name']])
         .self$type = xmlValue(.self$node[['type']])
         .self$date_received = xmlValue(.self$node[['date-received']]) 
         .self$date_completed = xmlValue(.self$node[['date-completed']]) 
         .self$biosource = .self$get_biosource()
      })
   )

Sample <- getRefClass("SampleRefClass")
      

#' Show
#' 
#' @family Node Sample
NULL
SampleRefClass$methods(
   show = function(prefix = ""){
      callSuper(prefix = prefix)
      cat(prefix, "  Sample name: ", .self$name, "\n", sep = "")
      cat(prefix, "  Sample type: ", .self$type, "\n", sep = "")
      cat(prefix, "  Sample date_received: ", .self$date_received, "\n", sep = "")
      cat(prefix, "  Sample date_completed: ", .self$date_completed, "\n", sep = "")
      cat(prefix, "  Sample biosource: ", .self$get_biosource(), "\n", sep = "")
   })  
   
#' Get Biosource description
#' 
#' @family Sample
#' @name SampleRefClass_get_biosource
#' @return character, possible empty
NULL
SampleRefClass$methods(
   get_biosource = function(){
      nd <- .self$node[['biosource']]
      if (!is.null(nd)) xmlAttrs(nd)[['name']] else ""
   })