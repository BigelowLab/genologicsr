# Exception.R

#' An Exception representation that sublcasses from NodeRefClass
#' 
#' @family Node
#' @field messgae
#' @include Node.R
#' @export
ExceptionRefClass <- setRefClass("ExceptionRefClass",
   contains = "NodeRefClass",
      fields = list(
         message = 'character'),
      methods = list(
         initialize = function(...){
            callSuper(...)
            .self$verbs <- "NONE"
            .self$message = get_childvalue(.self$node, 'message')
         },
      show = function(prefix = ""){
            callSuper(prefix = prefix)
            cat(prefix, "  Exception message: ", .self$message, "\n", sep = "")
         }
      )
   )
