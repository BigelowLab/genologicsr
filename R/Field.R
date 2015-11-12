#' A User Defined Field configuration representation that subclasses from NodeRefClass
#' 
#' @family Node
#' @field name character
#' @field type Field content type
#' @field attach_to_name character
#' @field is_required logical
#' @field attach_to_category character
#' @field first_preset_is_default_value logical
#' @field preset character (vector)
#' @include Node.R
#' @export
FieldRefClass <- setRefClass("FieldRefClass",
   contains = "NodeRefClass",
   fields = list(
      name = 'character',
      type = 'character',
      attach_to_name = 'character',
      is_required = 'logical',
      attach_to_category = 'character',
      first_preset_is_default_value = 'logical',
      preset = 'character'),
   methods = list(
      initialize = function(...){
         callSuper(...)
         if (is_xmlNode(.self$node)){
            .self$verbs <- c("GET", "PUT", 'BROWSE')
            .self$name <- .self$get_name()
            .self$type <- .self$get_type()
            .self$attach_to_name <- .self$get_childv('attach-to-name')
            .self$is_required <- .self$get_childv('is-required') == "true"
            .self$attach_to_category <- .self$get_childv('attach-to-category')
            .self$first_preset_is_default_value <- .self$get_childv('first-preset-is-default') == 'true'
            
            # presets are a little different since they might appear multiple times
            nm <- names(.self$node)
            ix <- nm %in% 'preset'
            if (any(ix)){
               p <- sapply(.self$node['preset'], function(x) XML::xmlValue(x))
            } else {
               p <- ""
            }
            .self$preset <- p
         
         } # has a node
      })
   )
   

#' Show the contents
#' @name FieldRefClass_show
#' @param prefix character - perhaps number of spaces to prefix the output 
#' @family Node
NULL
FieldRefClass$methods(
   show = function(prefix = ""){
      callSuper(prefix = prefix)
      cat(prefix, "  Field name: ",
         .self$name, "\n", sep = "")
      cat(prefix, "  Field type: ", 
         .self$type, "\n", sep = "")
      cat(prefix, "  Field attach_to_name: ", 
         .self$attach_to_name, "\n", sep = "")
      cat(prefix, "  Field is_required: ", 
         .self$is_required, "\n", sep = "")
      cat(prefix, "  Field attach_to_category: ", 
         .self$attach_to_category, "\n", sep = "")
      cat(prefix, "  Field first_preset_is_default_value: ", 
         .self$first_preset_is_default_value, "\n", sep = "")
      cat(prefix, "  Field preset: ", 
         paste(shQuote(.self$preset), collapse = " "), "\n", sep = "")
   }) #show


#' POST is disallowed for field configuration
#' @family Field
#' @name FieldRefClass_POST
NULL
FieldRefClass$methods(
   POST = function(){
      cat("FieldRefClass_POST in not a permitted transaction\n")
   })

#' DELETE is disallowed for field configuration
#' @family Field
#' @name FieldRefClass_DELETE
NULL
FieldRefClass$methods(
   DELETE = function(){
      cat("FieldRefClass_DELETE in not a permitted transaction\n")
   })
      
#' Get the value of the Udf type
#' 
#' @family Field
#' @name FieldRefClass_get_type
#' @return character the type name or "" if none
NULL
FieldRefClass$methods(
   get_type = function(){
      XML::xmlAttrs(.self$node)[['type']]
   })   

