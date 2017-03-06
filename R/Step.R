# Step.R

#' A Step representation that sublcasses from NodeRefClass
#' @include Node.R
#' @export
StepRefClass <- setRefClass("StepRefClass",
    contains = "NodeRefClass",
    fields = list(
        name = 'character',
        protocol_uri = 'character'
        ),
    methods = list(
       initialize = function(...){
          callSuper(...)
          .self$verbs <- c("GET", "POST", "BROWSE")
       },
    update = function(...){
        callSuper(...)
        a = xml_atts(.self$node)
        .self$name = a[['name']]
        .self$protocol_uri = a[['protocol-uri']]
        },

    show = function(prefix = ""){
       callSuper(prefix = prefix)
       cat(prefix, "  Step name: ", .self$name, "\n", sep = "")
       cat(prefix, "  Step protocol uri: ", .self$protocol_uri, "\n", sep = "")
     })
   )