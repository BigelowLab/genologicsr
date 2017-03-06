# Workflow.R

#' A Workflow representation that sublcasses from NodeRefClass
#' @include Node.R
#' @export
WorkflowRefClass <- setRefClass("WorkflowRefClass",
    contains = "NodeRefClass",
    fields = list(
        name = 'character',
        status = 'character'
        ),
    methods = list(
        initialize = function(...){
            callSuper(...)
            .self$verbs <- c("GET", "BROWSE")
            },
        update = function(...){
            callSuper(...)
            a <- xml_atts(.self$node)
            .self$name = a[['name']]
            .self$status = a[['status']]
            },
        show = function(prefix = ""){
            callSuper(prefix = prefix)
            cat(prefix, "  Workflow name: ", .self$name, "\n", sep = "")
            cat(prefix, "  Workflow status: ", .self$status, "\n", sep = "")
        })
   )

#' Get one or more protocols
#'
#' @name WorkflowRefClass_get_protocols
#' @param name NULL or charater vector of protocol names to retrieve
#' @param form character either 'uri' or 'Node'
#' @return vector of URI, list of Nodes or empty list/vector
WorkflowRefClass$methods(
    get_protocols = function(name = NULL, 
        form = c("uri", "Node")[2]){
    
    form <- tolower(form[1])
    PP <- .self$node[['protocols']]
    if (is.null(PP))  return(
            switch(form,
                'uri' = character(),
                'node' = list())
            )
    
    pp <- PP['protocol']
    aa <- lapply(pp, function(x) xml_atts(x))
    
    pp <- sapply(aa, '[[', 'uri')
    names(pp) <- sapply(aa, '[[', 'name')
    
    if (!is.null(name)){
        ix <- names(pp) %in% name
        pp <- pp[ix]
        if (length(pp) == 0){
            return(
                switch(form,
                    'uri' = character(),
                    'node' = list())
                )
        }
    }
    
    if (form == 'node') pp <- lapply(pp, function(x) .self$lims$GET(x))
       
    pp
    })   
    
    
#' Get one or more stages
#'
#' @name WorkflowRefClass_get_stages
#' @param name NULL or charater vector of stage names to retrieve
#' @param form character either 'uri' or 'Node'
#' @return vector of URI, list of Nodes or empty list/vector
WorkflowRefClass$methods(
    get_stages = function(name = NULL, 
        form = c("uri", "Node")[2]){
    
    form <- tolower(form[1])
    PP <- .self$node[['stages']]
    if (is.null(PP))  return(
            switch(form,
                'uri' = character(),
                'node' = list())
            )
    
    pp <- PP['stage']
    aa <- lapply(pp, function(x) xml_atts(x))
    
    pp <- sapply(aa, '[[', 'uri')
    names(pp) <- sapply(aa, '[[', 'name')
    
    if (!is.null(name)){
        ix <- names(pp) %in% name
        pp <- pp[ix]
        if (length(pp) == 0){
            return(
                switch(form,
                    'uri' = character(),
                    'node' = list())
                )
        }
    }
    
    if (form == 'node') pp <- lapply(pp, function(x) .self$lims$GET(x))
       
    pp
    })   

