# route.R


#' Create routing node
#' 
#' @seealso \url{https://www.genologics.com/files/permanent/API/latest/data_rt.html#routing}
#' @export
#' @return a routing node 
create_routing <- function(){
    XML::newXMLNode("routing", namespace = "rt",
      namespaceDefinitions = get_NSMAP()['rt'])
}

#' Create or update an artifact routing map
#'
#' @seealso \url{https://www.genologics.com/files/permanent/API/latest/data_rt.html#element_routing} 
#' @export
#' @param x a list of one or more artifacts as ArtifactsRefClass or xmlNode
#' @param workflow_uri from the link... 'The workflow to use for (un)assigning 
#'  the samples. Is not needed if stage URI is set. If stageURI is not 
#'  specified, the default stage in the workflow will be used for assignment.'
#' @param satge_uri from the link above 'The stage to assign the samples to. The #'  stage is tied to a workflow, so workflowURI is not needed if this attribute #'  is set.'
#' @param action character of either 'assign' or 'unassign'
#' @param routing xml structure of type 'routing' - if NULL a new one is created
#'  but if provided it is updated
#' @return xml routing node (new or updated) or NULL
create_routing_artifacts <- function(x, 
    workflow_uri = NULL, 
    stageuri = NULL,
    action = c("assign", "unassign")[1],
    routing = create_routing()){
 
    if (!is.list(x)) x <- list(x)
    if (inherits(x[[1]], 'NodeRefClass')) x <- lapply(x, '[[', 'node')   
 
    # first assemble the 
    xx <- lapply(x, 
        function(x){
            XML::newXMLNode('artifact',
                attrs = c(uri= trimuri(XML::xmlAttrs(x)[['uri']])))
        })
        
 
    kids <- XML::newXMLNode(action[1],
        attrs = c('workflow-uri' = workflow_uri, 'stage-uri' = stage_uri))
    kids <- XML::addChildren(kids, kids = xx)
    
    XML::addChildren(routing, kids = list(kids))
}
