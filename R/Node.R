# Node.R

NodeRefClass <- setRefClass("NodeRefClass",
   fields = list(
      lims = 'ANY', 
      uri = 'character',
      limsid = 'character',
      ns = 'ANY',
      node = 'ANY')
   )
 
Node <- getRefClass("NodeRefClass")  

#' Called when the object is instantiated.  NodeRefClass is a convenience wrapper
#' for XML::xmlNode resources used to work with the GLS API.
#'
#' @family Node
#' @name NodeRefClass_new
#' @param node either XML::xmlNode or a uri that points to such
#' @param lims LimsRefClass instance
#' @returns an instance of NodeRefClass
NULL
NodeRefClass$methods(
   initialize = function(node = NULL, lims = NULL, ...){
      if ( missing(node) || missing(lims) ) {
         return(callSuper(...))
      }
      
      if (!inherits(lims, "LimsRefClass")) 
         stop("NodeRefClass$initialize lims is must be of class LimsRefClass")
      
      .self$field('lims', lims)
      
      if (is_xmlNode(node)){
      
         .self$field('node', node)   
            
      } else if (is.character(node)){
      
         .self$field("node", lims$GET( trimuri(node[1]) ) )
         
      } else {
      
         stop("NodeRefClass$new: x must be either xmlNode or character uri")
      }
      
      .self$field('ns', XML::xmlNamespace(.self$node))
      
      atts <- XML::xmlAttrs(.self$node)
      .self$field('uri', trimuri(atts[['uri']]))
      .self$field('limsid', atts[['limsid']])
   
      callSuper(...)
   }) # new


#' Show the contents
#' @name NodeRefClass_show
#' @param prefix character - perhaps number of spaces to prefix the output 
#' @family Node
NULL
NodeRefClass$methods(
   show = function(prefix = ""){
      cat(prefix, "Reference Class: ", methods::classLabel(class(.self)), "\n", sep = "")
      .self$lims$show(prefix = paste0(prefix, "   "))
      cat(prefix, "  uri: ", .self$uri, "\n", sep = "")
      cat(prefix, "  namespace: ", paste(names(.self$ns), collapse = " "), "\n", sep = "")
      cat(prefix, "  node populated: ", is_xmlNode(.self$node), "\n", sep = "")
      cat(prefix, "  node children: ", paste(unames(.self$node), collapse = " "), "\n", sep = "")
   }) #show

   

#' Update the node information
#' 
#' @name NodeRefClass_update
#' @family Node
#' @param x XML::xmlNode reference
NULL
NodeRefClass$methods(
   update = function(x){
   
      if (is_exception(x)) stop("NodeRefClass$update input must be non-exception XML::xmlNode")
      
      .self$field('node', x)
      
       .self$field('ns', xmlNamespace(.self$node))
       
      atts <- xmlAttrs(.self$node)
      .self$field('uri', trimuri(atts[['uri']]))
      .self$field('limsid', atts[['limsid']])
   }) # update


#' GET un update of this node
#' 
#' @family Node
#' @name NodeRefClass_GET
#' @return logical if successful then TRUE
NULL
NodeRefClass$methods(
   GET = function(...){
      r <- .self$lims$GET(.self$uri, ...)
      ok <- TRUE
      if (!is_exception(r)) {
         .self$update(r)
      } else {
         ok <- FALSE
      }
      ok
   }) # GET
   
#' PUT this node
#'
#' @family Node
#' @name NodeRefClass_PUT
#' @return logical if successful then TRUE
NULL
NodeRefClass$methods(
   PUT = function(...){
      r <- .self$lims$PUT(.self, ...)
      ok <- TRUE
      if (!is_exception(r)) {
         .self$update(r)
      } else {
         cat("NodeRefClass: PUT exception\n")
         cat("NodeRefClass: node not updated becauee...\n")
         cat(xmlValue(r[['message']]), "\n")
         ok <- FALSE
      }
      ok
   }) # PUT
   
   
#' Retrieve a vector of unique child names
#'
#' @family Node
#' @name NodeRefClass_unames get the unique child names
#' @return a vector of unique children names
NULL
NodeRefClass$methods( 
   unames = function(){ 
      unames(.self$node) 
   })


#' Convert the node to string
#'
#' @name NodeRefClass_toString
#' @family Node
#' @return character representation of the XML::xmlNode
NodeRefClass$methods(
   toString = function(){
      gsub("\n","", XML::toString.XMLNode(.self$node))
   }) # toString
   
   
#' Get the contents of a UDF field
#' 
#' @family Node
#' @param name character one or more names 
#' @param as_type character, the data type to return: default, numeric, character, etc.
#' @return list of field values, assigned NA when the field is missing
NULL
NodeRefClass$methods(
   get_field = function(name){
      get_udfs(.self$node, name)       
   }) # get_field

#' Get an instance of NodeRefClass
Node <- getRefClass("NodeRefClass")

############## methods above
############## functions below



#' Set one or more UDF fields in an xmlNode
#' 
#' @export
#' @family Node
#' @param x XML::xmlNode
#' @param v a list of one or more udf vectors
#'    each udf vector must have \code{name}, \code{type} and \code{value}
#' @return the updated xmlNode
set_udfs <- function(x, v){


}

#' Extract a named list of udf vectors from an xmlNode
#' 
#' @export
#' @family Node
#' @param x XML::xmlNode possibly bearing udf fields
#' @return a named list of lists where each sublist is comprised of 
#'    \itemize{
#'       \item name character
#'       \item type character
#'       \item value data type varies
#'    }
#'  Or empty list if the xmlNode has no fields
extract_udfs <- function(x){
   stopifnot(is_xmlNode(x))
   ff <- x['field']
   if (is.null(ff)){
      r <- list()
   } else {
      r <- lapply(ff, function(x){
         att <- xmlAttrs(x)
         nm <- att[['name']]
         typ <- att[['type']]
         val <- as(xmlValue(x), udf_type2R(typ))
         list(name = nm, type = typ, value = val )
         })  
      names(r) <- sapply(r, "[[", "name")  
   }
   return(r)
}

#' Retrieve the values of one or more udfs
#' 
#' @export
#' @family Node
#' @param x XML::xmlNode object
#' @param name character one or more names 
#' @return named list of field values.  If no fields exists then NULL is return.
#' If a name is missing then 
get_udfs <- function(x, name){
   r <- extract_udfs(x) 
   lapply(r[name], '[[', 'value')
}
     

