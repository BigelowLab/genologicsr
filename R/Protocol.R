# Protocol.R

#' A Protocol representation that sublcasses from NodeRefClass
#' @include Node.R
#' @export
ProtocolRefClass <- setRefClass("ProtocolRefClass",
    contains = "NodeRefClass",
    fields = list(
        name = 'character',
        index = 'numeric'
        ),
    methods = list(
        initialize = function(...){
            callSuper(...)
            .self$verbs <- c("GET", "BROWSE")
            },
       update = function(...){
            callSuper(...)
            a = xml_atts(.self$node)
            .self$name = a[['name']]
            .self$index = as.numeric(a[['index']])
            },
        show = function(prefix = ""){
           callSuper(prefix = prefix)
           cat(prefix, "  Protocol name: ", .self$name, "\n", sep = "")
           cat(prefix, "  Protocol index: ", .self$index, "\n", sep = "")
         })
   )
