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
            if ('code' %in% names(atts)) .self$code = atts[['code']]
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

#' Test XML::xmlNode or NodeRefClass is an exception
#'
#' @export
#' @param x object to test
#' @param space the namespace to test
#' @param include_null logical, if TRUE assume that NULL is an exception
#' @return logical
is_exception <- function(x, space = 'exc', include_null = TRUE){


   if (inherits(x, "ExceptionRefClass")) return(TRUE)
   if (inherits(x, 'NodeRefClass')) x <- x$node
   if (include_null && is.null(x)) return(TRUE)
   is_xmlNode(x) && 
    ( ("exc" %in% names(XML::xmlNamespace(x)) ) || ("exception" %in% XML::xmlName(x)) )
}


#' Create an exception node
#' @export
#' @param status numeric status code if known, set to -1 otherwise
#' @param message character vector message
#' @param category character the exception category if known 
#' @return XML::xmlNode 
create_exception_node = function(message = 'Unspecified exception',
    status = -1, category = 'unknown'){
    x <- XML::newXMLNode("exception", 
      namespaceDefinitions = list(exc = get_NSMAP()[['exc']]), 
      #namespace = 'exc',
      attrs = list(code = as.character(status[1]), category = category[1]) )
    XML::addChildren(x, kids = list(XML::newXMLNode("message", message)) )
}
