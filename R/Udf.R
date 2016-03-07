# Udf.R

#' Create a UDF XML::xmlNode and possibly add it to a parent node
#'
#' Suppose you have the following UDF attributes and value...
#' x <- c(unit = "uL", type = "Numeric", name = "facs.Padding volume", value = 0.6)
#' Below is the expected appearance of the output ...
#' <udf:field unit="uL" type="Numeric" name="facs.Padding volume">0.6</udf:field>
#' 
#' @family Udf
#' @export
#' @param x a named character vector with at least "name", "type" and "unit" elements
#' but it may optionally include a "value" element.  If value is not provided then
#' the value in the returned node is empty.
#' @param namespace character, the xmlNamespace for the new xmlNode
#' @param parent optional xmlNode to whom to add the new node
#' @param ... further arguments for \code{\link{newXMLNode}} 
#' @return udf XML::xmlNode                           
create_udf_node <- function(x, namespace = 'udf', parent = NULL, ...) {
   
   #ns <- genologicsr::get_NSMAP()[namespace[1]]
   atts <- x[ names(x) %in% c("name", "type", "unit") ]
   
   newNode <- XML::newXMLNode("field", 
      attrs = atts, 
      namespace = namespace, 
      parent = parent, ...) 
   
   if ("value" %in% names(x)) XML::xmlValue(newNode) <- x[["value"]]
    
   return(newNode)        
}




#' Set one or more UDF fields in an xmlNode
#' 
#' @export
#' @family Node Udf
#' @param x XML::xmlNode
#' @param v a list of one or more udf vectors
#'    each udf vector must have \code{name}, \code{type} and \code{value}
#' @return the updated xmlNode
set_udfs <- function(x, v){
   
   if (!is.list(v)) v <- list(v)
   
   ok <- sapply(v, function(x) { 
      all(c("type", "name", "value") %in% names(x)) 
   })
   if (!all(ok)){
      stop("set_udfs: the input list of udf vector(s), v, must have at least 'type', 'name' and 'value' elements")
   }
   
   
   checkType <- function(typ, value){
      if(typ == "numeric") as.numeric(value) else  value
   }
   
   curUdfVals <- extract_udfs(x)
   newNames <- sapply(v, "[", "name")
   
   if (length(curUdfVals) == 0){              # 1 - all are new udfs to the node
      
      for (i in seq_along(v)) create_udf_node(v[[i]], parent = x)
      
   } else {                          # 2 - some or none are new udfs to the node
      
      curUdfNames <- names(curUdfVals)
      
      for (i in seq_along(v)){
      
         ix <- which(curUdfNames %in% newNames[i])
         
         if (length(ix) > 0) {
            # update an exisiting node
            typ <- unname(v[[i]][["type"]])
            name <- unname(v[[i]][["name"]])
            value <- checkType(typ, v[[i]][["value"]])
            cC <- x['field'][[ix[1]]]
            XML::xmlValue(cC) <- value
         } else{
            # or create a new one
            newNode <- genologicsr::create_udf_node(v[[i]], parent = x)
         }
         
      } # i-loop
      
   } #  either 1 or 2
   return(x)
}

#' Extract a named list of udf vectors from an xmlNode
#' 
#' @export
#' @family Node Udf
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
         att <- xml_atts(x)
         nm <- att[['name']]
         typ <- att[['type']]
         val <- as(xml_value(x), udf_type2R(typ))
         list(name = nm, type = typ, value = val )
         })  
      names(r) <- sapply(r, "[[", "name")  
   }
   return(r)
}

#' Retrieve the values of one or more udfs
#' 
#' @export
#' @family Node Udf
#' @param x XML::xmlNode object
#' @param name character one or more names 
#' @return named list of field values.  If no fields exists then NULL is return.
#' If a name is missing then 
get_udfs <- function(x, name){
   r <- extract_udfs(x) 
   lapply(r[name], '[[', 'value')
}
     

