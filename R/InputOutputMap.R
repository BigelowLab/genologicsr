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
         .self$post_process_uri <- x[['post-process-uri']]
         
         x <- xmlAttrs(.self$node[['output']])
         .self$output_uri <- trimuri(x[['uri']])
         .self$output_limsid <- x[['limsid']]
         .self$output_generation_type <- x[['output-generation-type']]
         .self$output_type <- x[['output_type']]
      })
   )
   
InputOutputMap <- getRefClass("InputOutputMapRefClass")
  
#' GET is disallowed for input-output-map
#' @family InputOutputMap
#' @name InputOutputMapRefClass_GET
NULL
ArtifactRefClass$methods(
   GET = function(){
      cat("ArtifactRefClass_GET in not a permitted transaction\n")
   })

#' PUT is disallowed for input-output-map
#' @family InputOutputMap
#' @name InputOutputMapRefClass_PUT
NULL
ArtifactRefClass$methods(
   PUT = function(){
      cat("ArtifactRefClass_PUT in not a permitted transaction\n")
   })
   
#' POST is disallowed for input-output-map
#' @family InputOutputMap
#' @name InputOutputMapRefClass_POST
NULL
ArtifactRefClass$methods(
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
          
      