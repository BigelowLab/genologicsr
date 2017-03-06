# Stage.R

#' A Stage representation that sublcasses from NodeRefClass
#' @include Node.R
#' @export
StageRefClass <- setRefClass("StageRefClass",
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
           cat(prefix, "  Stage name: ", .self$name, "\n", sep = "")
           cat(prefix, "  Stage index: ", .self$index, "\n", sep = "")
         })
   )
   
#' Get the workflow as uri or Node
#'
#' @name StageRefClass_get_workflow
#' @return uri, Node or NULL
StageRefClass$methods(
    get_workflow = function(form = c('uri', 'Node')[2]){
        form <- tolower(form[1])
        R <- NULL
        x = .self$node[['workflow']]
        if (!is.null(x)) {
            R <- xml_atts(x)[['uri']]
        } else {
            return(R)
        }
        if (form == 'node')  R <- .self$lims$GET(R)
        R
    })

#' Get the protocol as uri or Node
#'
#' @name StageRefClass_get_protocol
#' @return uri, Node or NULL
StageRefClass$methods(
    get_protocol = function(form = c('uri', 'Node')[2]){
        form <- tolower(form[1])
        R <- NULL
        x = .self$node[['protocol']]
        if (!is.null(x)) {
            R <- xml_atts(x)[['uri']]
        } else {
            return(R)
        }
        if (form == 'node')  R <- .self$lims$GET(R)
        R
    })

#' Get the Step as uri or Node
#'
#' @name StageRefClass_get_step
#' @return uri, Node or NULL
StageRefClass$methods(
    get_step = function(form = c('uri', 'Node')[2]){
        form <- tolower(form[1])
        R <- NULL
        x = .self$node[['step']]
        if (!is.null(x)) {
            R <- xml_atts(x)[['uri']]
        } else {
            return(R)
        }
        if (form == 'node')  R <- .self$lims$GET(R)
        R
    })



