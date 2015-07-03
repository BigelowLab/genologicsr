InputOutputMapRefClass <- setRefClass("InputOutputMapRefClass",
   contains = "NodeRefClass",
   
   fields = list(
      input_uri = 'character',
      input_limsid = 'character',
      post_process_uri = 'character',
      
      output_uri = 'character',
      output_limsid = 'character',
      output_generation_type = 'character',
      output_type = 'character'),
      
   methods = list(
      initialize = function(...){
         callSuper(...)
         
         x <- xmlAttrs(.self$node[['input']])
         .self$input_uri <- trimuri(x[['uri']])
         .self$input_limsid <- x[['limsid']]
         .self$post_process_uri <- trimuri(x[['post-process-uri']])
         
         x <- xmlAttrs(.self$node[['output']])
         .self$output_uri <- trimuri(x[['uri']])
         .self$output_limsid <- x[['limsid']]
         .self$output_generation_type <- x[['output-generation-type']]
         .self$output_type <- x[['output-type']]
      })
   )
   
InputOutputMap <- getRefClass("InputOutputMapRefClass")


#' Show
#' 
#' @family Node InputOutputMap
NULL
InputOutputMapRefClass$methods(
   show = function(prefix = ""){
      callSuper(prefix = prefix)
      cat(prefix, "  InputOutputMap input_uri: ", .self$input_uri, "\n", sep = "")
      cat(prefix, "  InputOutputMap input_limsid: ", .self$input_limsid, "\n", sep = "")
      cat(prefix, "  InputOutputMap post_process_uri: ", .self$post_process_uri, "\n", sep = "")
      cat(prefix, "  InputOutputMap output_uri: ", .self$output_uri, "\n", sep = "")
      cat(prefix, "  InputOutputMap output_limsid: ", .self$output_limsid, "\n", sep = "")      
      cat(prefix, "  InputOutputMap output_generation_type: ", .self$output_generation_type, "\n", sep = "")
      cat(prefix, "  InputOutputMap output_type: ", .self$output_type, "\n", sep = "")
   })
     
#' GET is disallowed for input-output-map
#' @family InputOutputMap
#' @name InputOutputMapRefClass_GET
NULL
InputOutputMapRefClass$methods(
   GET = function(){
      cat("ArtifactRefClass_GET in not a permitted transaction\n")
   })

#' PUT is disallowed for input-output-map
#' @family InputOutputMap
#' @name InputOutputMapRefClass_PUT
NULL
InputOutputMapRefClass$methods(
   PUT = function(){
      cat("ArtifactRefClass_PUT in not a permitted transaction\n")
   })
   
#' POST is disallowed for input-output-map
#' @family InputOutputMap
#' @name InputOutputMapRefClass_POST
NULL
InputOutputMapRefClass$methods(
   POST = function(){
      cat("ArtifactRefClass_POST in not a permitted transaction\n")
   })

#' DELETE is disallowed for artifacts
#' @family InputOutputMap
#' @name InputOutputMapRefClass_DELETE
NULL
InputOutputMapRefClass$methods(
   DELETE = function(){
      cat("InputOutputMapRefClass_DELETE in not a permitted transaction\n")
   })
          
      