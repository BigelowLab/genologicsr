InstrumentRefClass <- setRefClass("InstrumentRefClass",
   contains = "NodeRefClass",
   fields = list(
      name = "character",
      type = "character"),
   methods = list( 
      initialize = function(...){
         callSuper(...)
         .self$verbs <- c("GET", "BROWSE")
         .self$update()
      },
   update = function(){
      callSuper(.self$node)
      .self$name = XML::xmlValue(.self$node[['name']]) 
      .self$type = XML::xmlValue(.self$node[['type']]) 
      },
   show = function(prefix = ""){
      callSuper(prefix = prefix)
      cat(prefix, "  Instrument name: ", .self$name, "\n", sep = "")
      cat(prefix, "  Instrument type: ", .self$type, "\n", sep = "")
      }   )
   )

#' POST is disallowed for instruments
#' @family Instrument
#' @name InstrumentRefClass_POST
NULL
InstrumentRefClass$methods(
   POST = function(){
      cat("InstrumentRefClass_POST in not a permitted transaction\n")
   })

#' PUT is disallowed for instruments
#' @family Instrument
#' @name InstrumentRefClass_PUT
NULL
InstrumentRefClass$methods(
   PUT = function(){
      cat("InstrumentRefClass_PUT in not a permitted transaction\n")
   })