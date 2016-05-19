# Exception.R

#' An Exception representation that sublcasses from NodeRefClass
#' 
#' @family Node
#' @field category character, the category (if any)
#' @field code character, the error code (if any)
#' @field message character, the value of the message
#' @field suggested_actions character, (if any)
#' @include Node.R
#' @export
ExceptionRefClass <- setRefClass("ExceptionRefClass",
   contains = "NodeRefClass",
      fields = list(
         category = 'character',
         code = 'character',
         message = 'character',
         suggested_actions = 'character'),
      methods = list(
         initialize = function(...){
            x <- list(...)
            .self$node <- x[[1]]
            .self$verbs <- "NONE"
            atts <- xml_atts(.self$node)
            if ('category' %in% names(atts)) .self$category = atts[['category']]
            if ('code' %in% names(atts)) .self$category = atts[['code']]
            if ('message' %in% names(.self$node))   
               .self$message = get_childvalue(.self$node, 'message')
            if ('suggested-actions' %in% names(.self$node))
               .self$suggested_actions = get_childvalue(.self$node, 'suggested_actions')
         },
      show = function(prefix = ""){
            callSuper(prefix = prefix)
            cat(prefix, "  Exception category: ", .self$category, "\n", sep = "")
            cat(prefix, "  Exception code: ", .self$code, "\n", sep = "")
            cat(prefix, "  Exception message: ", .self$message, "\n", sep = "")
            cat(prefix, "  Exception suggested-actions: ", .self$suggested_actions, "\n", sep = "")
         }
      )
   )

#' Create an exception node
#' @export 
#' @param message character vector message
#' @return XML::xmlNode 
create_exception_node = function(message = 'Unspecified exception'){
    x <- XML::newXMLNode("exception", 
      namespaceDefinitions = get_NSMAP()[['exc']], 
      namespace = 'exc')
    XML::addChildren(x, kids = list(XML::newXMLNode("message", message)) )
}
