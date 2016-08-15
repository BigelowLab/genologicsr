# Processtype.R

#' A Processtype representation that subclasses from NodeRefClass
#' 
#' @family Node
#' @field name character, name if any
#' @include Node.R
#' @export
ProcessTypeRefClass <- setRefClass("ProcessTypeRefClass",
   contains = 'NodeRefClass',
   fields = list(
      name = 'character',
      fields = 'ANY'),
   methods = list(
      show = function(prefix = ""){
         callSuper(prefix = prefix)
         cat(prefix, "  Process type name: ", .self$name, "\n", sep = "")
         cat(prefix, "  Process type fields: ", 
            paste(.self$fields, collapse = " "), "\n", sep = "") 
         
      },
      get_field_names = function(){
         sapply(.self$node['field-definition'], 
            function(x){
               xml_atts(x)[['name']]
            })
      },
      initialize = function(...){
         callSuper(...)
         .self$verbs <- c("GET", "BROWSE")
         .self$update()
         },
      update = function(...){
         callSuper(...)
         .self$name <- xml_atts(.self$node)[['name']]
         .self$fields <- .self$get_field_names()
      })
   )# setRefClass
         

#' Retrieve a named list of fields
#'
#' @family Processtype
#' @name ProcessTypeRefClass_get_fields
#' @return a named list of FieldRefClass or  NULL
NULL
ProcessTypeRefClass$methods(
   get_fields = function(){
      ff <- .self$node['field-definition']
      if (is.null(ff)) return(NULL)
      ff_uri <- sapply(ff, function(x) xml_atts(x)[['uri']])
      FF <- lapply(ff_uri, function(x) .self$lims$GET(x))
      names(FF) <- sapply(FF, "[[", "name")
      FF
   })


#' Retrieve a named list of process-inputs
#'
#' @family Processtype
#' @name ProcessTypeRefClass_get_inputs
#' @return a named list of inputs NULL
NULL
ProcessTypeRefClass$methods(
   get_inputs = function(){
      pinputs <- .self$node['process-input']
      if (is.null(pinputs)) return(NULL)
      inputs <- lapply(pinputs, function(x) {
            list(
               artifact_type = xml_value(x[['artifact-type']]),
               display_name = xml_value(x[['display-name']]),
               remove_working_flag = xml_value(x[['remove-working-flag']])
               )
            }
         )
      names(inputs) <- sapply(inputs, '[[', 'display_name')
      return(inputs)
   })

#' Retrieve a named list of process-output
#'
#' @family Processtype
#' @name ProcessTypeRefClass_get_outputs
#' @return a named list of outputs NULL
NULL
ProcessTypeRefClass$methods(
   get_outputs = function(){
      poutputs <- .self$node['process-output']
      if (is.null(poutputs)) return(NULL)
      out <- lapply(poutputs, function(x) {
         list(
            artifact_type = xml_value(x[['artifact-type']]),
            display_name = xml_value(x[['display-name']]),
            output_generation_type = xml_value(x[['output-generation-type']]),
            variablility_type = xml_value(x[['variability-type']]),
            number_of_outputs = xml_value(x[['number-of-outputs']]),
            output_name = xml_value(x[['output-name']])
            )
         }
      )
      names(out) <- sapply(out, '[[', 'display_name')
      return(out)
   })
