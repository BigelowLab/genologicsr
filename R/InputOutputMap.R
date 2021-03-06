#' An Input-Output-Map representation that subclasses from NodeRefClass
#' 
#' @family Node
#' @field input_uri character 
#' @field input_limsid character 
#' @field post_process_uri character 
#' @field output_uri character 
#' @field output_limsid character
#' @field output_generation_type character 
#' @field output_type character
#' @include Node.R
#' @export
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
         
         .self$verbs <- c("FOO")
         .self$input_uri <- ''
         .self$input_limsid <- ''
         .self$post_process_uri <- ''
         .self$output_uri <- ''
         .self$output_limsid <- ''
         .self$output_generation_type <- ''
         .self$output_type <- '' 
                      
         ind <- .self$node[['input']]
         if (!is.null(ind)){
             x <- xml_atts(ind)
             if (!is.null(x)){
                .self$input_uri <- trimuri(x[['uri']])
                .self$input_limsid <- x[['limsid']]
                .self$post_process_uri <- trimuri(x[['post-process-uri']])
             }
         }
                  
         ond <- .self$node[['output']]
         if (!is.null(ond)){
            x <- xml_atts(ond)
            if (!is.null(x)){
               .self$output_uri <- trimuri(x[['uri']])
               .self$output_limsid <- x[['limsid']]
               .self$output_generation_type <- x[['output-generation-type']]
               .self$output_type <- x[['output-type']]
            }
         }
            
      })
   )
   
InputOutputMap <- getRefClass("InputOutputMapRefClass")


#' Show
#' 
#' @family Node InputOutputMap
#' @name InputOutputMapRefClass_show 
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
      cat("InputOutputMapRefClass_GET in not a permitted transaction\n")
   })

#' PUT is disallowed for input-output-map
#' @family InputOutputMap
#' @name InputOutputMapRefClass_PUT
NULL
InputOutputMapRefClass$methods(
   PUT = function(){
      cat("InputOutputMapRefClass_PUT in not a permitted transaction\n")
   })
   
#' POST is disallowed for input-output-map
#' @family InputOutputMap
#' @name InputOutputMapRefClass_POST
NULL
InputOutputMapRefClass$methods(
   POST = function(){
      cat("InputOutputMapClass_POST in not a permitted transaction\n")
   })

#' DELETE is disallowed for artifacts
#' @family InputOutputMap
#' @name InputOutputMapRefClass_DELETE
NULL
InputOutputMapRefClass$methods(
   DELETE = function(){
      cat("InputOutputMapRefClass_DELETE in not a permitted transaction\n")
   })
          

################################################################################

#' Create an 'input-output-map' node for Analyte - useful when 
#' programmatically running a process. 
#' 
#' @export
#' @seealso \url{https://genologics.zendesk.com/entries/23659973-Running-a-Process}
#' @param inputartifacturi character vector of input artifact URIs
#' @param container_uri character vector, the uri of the destination container
#' @param placement character vector, the 'A:1' form well placement
#' @param output_type character, you shouldn't have to change this
#' @return an XML::xmlNode of type input-output-map for 'Analyte'
create_iom_analyte <- function(inputartifacturi, 
   container_uri = "container_uri", 
   placement = "A:1",
   output_type = 'Analyte'){
   
   input <- XML::newXMLNode("input", attrs = list("uri" = inputartifacturi))
   output <- XML::newXMLNode("output", attrs = list(type = output_type),
      .children = list(XML::newXMLNode("location", 
         .children = list(
            XML::newXMLNode("container", attrs = list("uri" = container_uri)),
            XML::newXMLNode("value", placement) )) ) )

   XML::newXMLNode("input-output-map", attrs = list("shared" = "false"),
      .children = c(input, output))
}


#' Create an 'input-output-map' node for a result file - useful when 
#' programmatically running a process.  One input = one output
#' 
#' @seealso \url{https://genologics.zendesk.com/entries/23659973-Running-a-Process}
#' @export
#' @param inputartifacturi character URI for input artifact
#' @param output_type character, you shouldn't have to change this
#' @return an XML::xmlNode of type input-output-map
create_iom_resultfile <- function(inputartifacturi, output_type = 'ResultFile'){
   
   inputs <-  XML::newXMLNode("input", attrs = list(uri = inputartifacturi)) 
   outputs <- XML::newXMLNode("output", attrs = list(type = output_type))
   
   XML::newXMLNode("input-output-map",
      attrs = list(shared = 'false'),
      .children = c(inputs, outputs))
}

#' Create an 'input-output-map' node for a shared result file - useful when 
#' programmatically running a process. Multiple inputs = one output
#' 
#' @seealso \url{https://genologics.zendesk.com/entries/23659973-Running-a-Process}
#' @export
#' @param inputartifacturi character vector of URIs
#' @param output_type character, you shouldn't have to change this
#' @return an XML::xmlNode of type input-output-artifact
create_iom_shared_resultfile <- function(inputartifacturi, output_type = 'ResultFile'){
   
   inputs <- lapply(inputartifacturi, function(x) XML::newXMLNode("input", attrs = list("uri" = x)) )
   outputs <- XML::newXMLNode("output", attrs = list(type = output_type))
   
   XML::newXMLNode("input-output-map",
      attrs = list(shared = 'true'),
      .children = c(inputs, outputs))
}
