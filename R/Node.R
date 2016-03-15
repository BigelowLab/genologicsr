# Node.R

#' The root class that wraps XML::xmlNode
#' 
#' @family Node
#' @field lims a LimsRefClass object (possibly NULL)
#' @field uri character the uri of the node (possibly "")
#' @field limsid character the limsid of the node (possibly "")
#' @field ns character the XML::xmlNamespace object (possibly NULL)
#' @field node XML::xmlNode external pointer to the representation (possibly NULL)
#' @field verbs character vector of 'GET', 'PUT', 'POST', 'BROWSE', and 'DELETE'
#' @include Lims.R
#' @export
NodeRefClass <- setRefClass("NodeRefClass",
   fields = list(
      lims = 'ANY', 
      uri = 'character',
      limsid = 'character',
      ns = 'ANY',
      node = 'ANY',
      verbs = 'character')
   )

Node <- getRefClass("NodeRefClass")  

#' Called when the object is instantiated.  NodeRefClass is a convenience wrapper
#' for XML::xmlNode resources used to work with the GLS API.
#'
#' @family Node
#' @name NodeRefClass_new
#' @param node either XML::xmlNode or a uri that points to such
#' @param lims LimsRefClass instance
#' @return an instance of NodeRefClass
NULL
NodeRefClass$methods(
   initialize = function(node = NULL, lims = NULL, ...){
      if ( missing(node) || missing(lims) ) {
         return(callSuper(...))
      }
         
      callSuper(...)
      .self$verbs <- c("PUT", "GET", "POST", "DELETE", "BROWSE")
      
      if (!inherits(lims, "LimsRefClass") && !is.null(lims) ) 
         stop("NodeRefClass$initialize lims is must be of class LimsRefClass or NULL")
      
      .self$field('lims', lims)
      
      if (is_xmlNode(node)){
      
         .self$field('node', node)   
            
      } else if (is.character(node)){
      
         if (inherits(lims, 'LimsRefClass')){
            .self$field("node", lims$GET( trimuri(node[1]) ) )
         } else {
            stop("NodeRefClass$new: if node is a uri then lims must not be NULL")
         }
         
      } else {
      
         stop("NodeRefClass$new: x must be either xmlNode or character uri")
      }
      
      .self$update()
      
   }) # new


#' Show the contents
#' @name NodeRefClass_show
#' @param prefix character - perhaps number of spaces to prefix the output 
#' @family Node
NULL
NodeRefClass$methods(
   show = function(prefix = ""){
      cat(prefix, "Reference Class: ", methods::classLabel(class(.self)), "\n", sep = "")
      cat(prefix, "  Node uri: ", .self$uri, "\n", sep = "")
      cat(prefix, "  Node children: ", paste(.self$unames(), collapse = " "), "\n", sep = "")
   }) #show

 

#' Update the node information
#' 
#' @name NodeRefClass_update
#' @family Node
#' @param x XML::xmlNode reference
NULL
NodeRefClass$methods(
   update = function(x){
   
      if (missing(x)) x <- .self$node
      
      if (!is_xmlNode(x) || is_exception(x)) {
         cat("NodeRefClass$update input must be non-exception XML::xmlNode\n")
         print(str(x))
         return(invisible(NULL))
      }

      .self$field('node', x)
      .self$field('ns', XML::xmlNamespace(.self$node))
       
      atts <- xml_atts(.self$node)
      if('uri' %in% names(atts)) .self$field('uri', trimuri(atts[['uri']]))
      if ('limsid' %in% names(atts)){
        .self$field('limsid', atts[['limsid']])
      } else {
        .self$field('limsid',.self$get_limsid())
      }
   }) # update

#' Retrieve the limsid (if any)
#'
#' @family Node
#' @name NodeRefClass_get_limsid
#' @return character (possibly an empty string)
NULL
NodeRefClass$methods(
    get_limsid = function(){
        basename(.self$uri)
    })
    
#' Determine if http transactions (GET, PATCH, POST, HEAD, PUT, and DELETE) 
#' are possible for this Node
#' 
#' @family Node
#' @name NodeRefClass_has_lims
#' @return logical
NULL
NodeRefClass$methods(
   has_lims = function(){
      !is.null(.self$lims)
   }) #has_lims


#' GET an update of this node
#' 
#' @family Node
#' @name NodeRefClass_GET
#' @return logical if successful then TRUE
NULL
NodeRefClass$methods(
   GET = function(...){
      if (!("GET" %in% .self$verbs)) {
         stop(methods::classLabel(class(.self)), " GET is not allowed")
      }
      if (!.self$has_lims()) stop("NodeRefClass$GET lims not available for GET")
      r <- .self$lims$GET(.self$uri, ..., asNode = FALSE)
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
      if (!("PUT" %in% .self$verbs)) {
         stop(methods::classLabel(class(.self)), " PUT is not allowed")
      }
      if (!.self$has_lims()) stop("NodeRefClass$PUT lims not available for PUT")
      
      # file:file, udf:field, udf:type, ri:externalid
      # must be bounded so we have to add to each of these
      # xmlns:file="http://genologics.com/ri/file"
      # udf:type xmlns:udf="http://genologics.com/ri/userdefined"
      # udf:field xmlns:udf="http://genologics.com/ri/userdefined"
      # xmlns:ri="http://genologics.com/ri"
      # see XML::newXMLNamespace and misc::get_NSMAP
      #.self$add_namespaces()
      
      r <- .self$lims$PUT(.self, ...)
      ok <- TRUE
      if (!is_exception(r)) {
         .self$update(r)
      } else {
         cat("NodeRefClass: PUT exception\n")
         cat(xml_value(r[['message']]), "\n")
         ok <- FALSE
      }
      ok
   }) # PUT

#' DELETE this node 
#'
#' @family Node
#' @name NodeRefClass_DELETE
#' @return logical if successful then TRUE
NULL
NodeRefClass$methods(
   DELETE = function(...){
      if (!("DELETE" %in% .self$verbs)) {
         stop(methods::classLabel(class(.self)), " DELETE is not allowed")
      }

      if (!.self$has_lims()) stop("NodeRefClass$GET lims not available for DELETE")
      r <- .self$lims$DELETE(.self$node, ...)
      ok <- TRUE
      if (!is_exception(r)) {
         .self$node <- NULL
         .self$lims <- NULL
      } else {
         cat("NodeRefClass: DELETE exception\n")
         cat(xml_value(r[['message']]), "\n")
         ok <- FALSE
      }
      ok
   }) # PUT
   
   
#' POST this node
#' 
#' @family Node
#' @name NodeRefClass_POST
#' @return logical, TRUE if successful
NULL
NodeRefClass$methods(
   POST = function(...){
      if (!("POST" %in% .self$verbs)) {
         stop(methods::classLabel(class(.self)), " POST is not allowed")
      }
      if (!.self$has_lims()) stop("NodeRefClass$POST lims not available for DELETE")
      r <- .self$lims$POST(.self$node, ...)
      ok <- TRUE
      if (!is_exception(r)) {
         .self$update(r)
      } else {
         cat("NodeRefClass: POST exception\n")
         cat("NodeRefClass: node not POSTed because...\n")
         cat(xml_value(r[['message']]), "\n")
         ok <- FALSE
      }
      ok
 
   }) #POST

#' BROWSE this node in a browser if in interactive session
#'
#' @family Node
#' @name NodeRefClass_BROWSE
#' @param ... further arguments for httr::BROWSE
NULL
NodeRefClass$methods(
   BROWSE = function(...){
      if (!("BROWSE" %in% .self$verbs)) {
         stop(methods::classLabel(class(.self)), " BROWSE is not allowed")
      }
      httr::BROWSE(.self$uri,...)
   })

#' Convert the XML node to string
#'
#' @family Node
#' @name NodeRefClass_toString
#' @return text reprepresentation of the node's xml data
NULL
NodeRefClass$methods(
   toString = function(){
      return(xmlString(.self$node))
   })
 

#' Return a character vector of required namespace names
#'
#' @family Node
#' @name NodeRefClass_required_namespaces
#' @return character vector (possible empty)
NULL
NodeRefClass$methods(
   required_namespaces = function(){
      return(character())
   })

#' Add namespaces to the XML::xmlNode as required - useful before a PUT
#'
#' @family Node
#' @name NodeRefClass_add_namespaces
#' @return the NodeRefClass invisibly
NULL
NodeRefClass$methods(
   add_namespaces = function(){
      # each class 'knows' what is required - if none then return
      need_ns <- .self$required_namespaces()
      if (length(need_ns) <= 0) return(invisible(.self))
      # see what exists already - if needed already then then return
      current_ns <- names(XML::xmlNamespaces(.self$node, simplify = TRUE))
      names_ns <- need_ns[!(need_ns %in% current_ns)]
      if (length(names_ns) <= 0) return(invisible(.self))
      # other wise add the new namespaces - this feels awkward
      # why can't we simple have XML::addNamespaces ?
      add_ns <- get_NSMAP()[names_ns]
      NS <- XML::newXMLNamespace(.self$node, add_ns)
      invisible(.self)
   })
   

#' Retrieve a vector of unique child names
#'
#' @family Node
#' @name NodeRefClass_unames
#' @return a vector of unique children names
NULL
NodeRefClass$methods( 
   unames = function(){ 
      if (is_xmlNode(.self$node)) unique(names(.self$node)) else ""
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
   
#' Determine if this node has a child, test by name of XML::xmlNode reference
#'
#' @family Node
#' @name NodeRefClass_has_child
#' @param x one or more names to test or one or more XML::xmlNode objects
#' @return logical vector, named if \code{x} is character
NodeRefClass$methods(
   has_child = function(x){
      if (inherits(x, "character")){
         u <- .self$unames()
         ok <- x %in% u
         names(ok) <- x
      } else {
         # this is what I want, but %in% doesn't operate on lists
         # x <- x %in% xmlChildren(.self$node)
         # so instead we loop through comparing xmlparent this this node
         ok <- sapply(x, function(x, p=NULL) {
               identical(p, XML::xmlParent(x))
            }, p = .self$node)   
      }
      ok
   })
   
#' Get the contents of a UDF field
#' 
#' @family Node
#' @name NodeRefClass_get_field
#' @param name character one or more names 
#' @param as_type character, the data type to return: default, numeric, character, etc.
#' @return list of field values, assigned NA when the field is missing
NULL
NodeRefClass$methods(
   get_field = function(name){
      get_udfs(.self$node, name)       
   }) # get_field


#' Get the value of the type field or ""
#' 
#' @family Node
#' @name NodeRefClass_get_type
#' @return character the type name or "" if none
NULL
NodeRefClass$methods(
   get_type = function(){
      get_childvalue(.self$node, "type")
   })

#' Get the value of the name field or ""
#' 
#' @family Node
#' @name NodeRefClass_get_type
#' @return named character vector of the name or "" if none
NULL
NodeRefClass$methods(
   get_name = function(){
      get_childvalue(.self$node, "name")
   })
   
#' Get the value of the value of a child or ""
#' 
#' @family Node
#' @name NodeRefClass_get_childv
#' @param names one or more child names
#' @return named character vector of the name or "" if none
NULL
NodeRefClass$methods(
   get_childv = function(names){
      get_childvalue(.self$node, names)
   })
   
   
#' Set the value of the value of  one or more UDF children.  If the
#' field does not exist it is created and added, otherwise it is updated.
#' 
#' @family Node
#' @name NodeRefClass_set_udfs
#' @param v a list of one or more udf vectors
#'    each udf vector must have \code{name}, \code{type} and \code{value}
#' @return a reference to this (updated) NodeRefClass object
NULL
NodeRefClass$methods(
   set_udfs = function(v){
      .self$node <- genologicsr::set_udfs(.self$node, v)
      return(.self)
   })   


#' Extract the UDFs into a list of udf vectors each with 'name', 'type' and 'value' elements
#'
#' @family Node Udf
#' @name NodeRefClass_get_udfs
#' @return a named list of lists where each sublist is comprised of 
#'    \itemize{
#'       \item name character
#'       \item type character
#'       \item value data type varies
#'    }
#'  Or empty list if the xmlNode has no fields
NULL
NodeRefClass$methods(
   get_udfs = function(){
      genologicsr::extract_udfs(.self$node)
   })


############## methods above
############## functions below

#' Retrieve the value of a child node(s)
#'
#' @export
#' @param x XML::xmlNode or NodeRefClass
#' @param name the name of the child(ren)
#' @return named character vector of child values, possibly ""
get_childvalue <- function(x, name){

   sapply(name, function(nm,x=NULL){
         if (nm %in% names(x)) xml_value(x[[name]]) else ""
      }, x = if (inherits(x, 'NodeRefClass')) x$node else x )

}