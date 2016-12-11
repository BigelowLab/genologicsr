# Container.R

#' A Container representation that subclasses from NodeRefClass
#' 
#' @family Node
#' @field name character, name if any
#' @field state character, the state of the container
#' @field type character, type if any
#' @include Node.R
#' @export
ContainerRefClass <- setRefClass("ContainerRefClass",
   contains = "NodeRefClass",
   fields = list(
      name = "character",
      state = "character",
      type = "character"),
   methods = list( 
        initialize = function(...){
            callSuper(...)
            .self$verbs <- c("GET", "PUT", 'DELETE', 'BROWSE')   
            },
        update = function(...){
           callSuper(...)
           .self$name = xml_value(.self$node[['name']]) 
           .self$type = xml_atts(.self$node[['type']])[['name']]
           .self$state = xml_value(.self$node[['state']])    
            })
   )
   
Container <- getRefClass("ContainerRefClass")

#' Show
#' 
#' @family Node Process
#' @name ContainerRefClass_show
NULL
ContainerRefClass$methods(
   show = function(prefix = ""){
      callSuper(prefix = prefix)
      cat(prefix, "  Container name: ", .self$name, "\n", sep = "")
      cat(prefix, "  Container type: ", .self$type, "\n", sep = "")
      #cat(prefix, "  Container state: ", .self$state, "\n", sep = "")
      #cat(prefix, "  Container occupied wells: ", .self$n_occupied(), 
      #   " (", .self$n_empty(), " empty)\n", sep = "")
      cat(sprintf("%s  Container state: %s [%i of %i occupied]\n", 
         prefix, .self$state, .self$n_occupied(), .self$n_wells()) )
   })  


#' Return a character vector of required namespace names
#'
#' @family Container
#' @name ContainerRefClass_required_namespaces
#' @return character vector
NULL
ContainerRefClass$methods(
   required_namespaces = function(){
      c(field = "udf")
   })

#' POST is disallowed for containers
#' @family Container
#' @name ContainerRefClass_POST
NULL
ContainerRefClass$methods(
   POST = function(){
      cat("ContainerRefClass_POST in not a permitted transaction\n")
   })
   
#' Retrieve the occupied well count
#'
#' @family Container
#' @name ContainerRefClass_n_occupied
#' @return numeric
NULL
ContainerRefClass$methods(
   n_occupied = function(){
      nd <- .self$node[['occupied-wells']]
      if (!is.null(nd)) {
         x <- as.numeric(xml_value(nd))
      } else {
         x <- NA
      }
      x
   }) #occupied_wells
   
#' Retrieve the empty well count
#'
#' @family Container
#' @name ContainerRefClass_n_empty
#' @return numeric
NULL
ContainerRefClass$methods(
   n_empty = function(){
      .self$n_wells() - .self$n_occupied()
   }) #empty_wells
   
#' Retrieve the well count (occupied + empty)
#'
#' @family Container
#' @name ContainerRefClass_n_wells
#' @return numeric
NULL
ContainerRefClass$methods(
   n_wells = function(){
      if ( (length(.self$type) == 0) || (nchar(.self$type) == 0) ) return(NA)
      switch(.self$type,
         '384 well plate' = 384,
         '96 well plate' = 96,
         '12 well plate' = 12,
         1)
   }) #n_wells  

#' Retrieve a named vector of container type attributes (uri and name)
#' 
#' @family Container
#' @name ContainerRefClass_get_type
#' @return a named vector of uri and name, or NULL
NULL
ContainerRefClass$methods(
   get_type = function(){
      x <- if (is.null(.self$node[['type']])) NULL else xml_atts(.self$node[['type']])
      x
   })


#' Test if one or more wells are occupied
#' 
#' @family Container
#' @name ContainerRefClass_well_occupied
#' @param placement character vector of well names
#' @return logical named vector
NULL
ContainerRefClass$methods(
    well_occupied = function(placement){
        pp <- .self$get_placements()
        p <- A01(placement, form = 'A:1')
        ix <- p %in% names(pp)
        names(ix) <- placement
        ix
    })  

#' Retrieve a named vector of placements uri
#' 
#' @family Container
#' @name ContainerRefClass_get_placements
#' @param placement character vector of well names (A:1 etc.) or NULL to get all
#' @return a named vector of placements, possibly an empty character vector
NULL
ContainerRefClass$methods(
   get_placements = function(placement = NULL){
      if ( !.self$has_child("placement") ) return(character())
      puri <- sapply(.self$node['placement'], function(x) xml_atts(x)[['uri']])
      names(puri) <- sapply(.self$node['placement'], xml_value)
      if (!is.null(placement)) {
         placement <- A01(placement, form = "A:1")
         ix <- placement %in% names(puri)
         if (!all(ix)){
            cat("ContainerRefClass$get_placements name(s) not found:", paste(placement, collapse = " "), "\n")
            return(character())
         }
         puri <- puri[placement]
      }
      invisible(puri)
   })

#' Retrieve a named list ArtifactRefClass objects
#' 
#' @family Container
#' @name ContainerRefClass_get_artifacts
#' @param placement well name such as 'A:1' or NULL to retrieve all
#' @return a named list of ArtifactRefClass objects or NULL
NULL
ContainerRefClass$methods(
   get_artifacts = function(placement = NULL){
      puri <- .self$get_placements(placement = placement) 
      if (length(puri) == 0) return(NULL)
      AA <- .self$lims$batchretrieve(puri,rel = 'artifacts', asNode = TRUE)
      # we could copy names over from puri, but it might be better to dig them out
      names(AA) <- sapply(AA, function(x) xml_value(x$node[['location']]))
      invisible(AA)
   })
   

#' Retrieve a named list of artifact XML:xmlNode or SampleRefClass
#' 
#' @family Container
#' @name ContainerRefClass_get_samples
#' @param placement well name such as 'A:1' or NULL to retrieve all
#' @param asNode logical if TRUE cast the result to SampleRefClass
#' @return a named vector of sample XML:xmlNode or SampleRefClass objects
NULL
ContainerRefClass$methods(
   get_samples = function(placement = NULL, asNode = TRUE){
      puri <- .self$get_placements(placement = placement)
      if (length(puri) == 0) return(NULL)
      AA <- .self$lims$batchretrieve(puri, rel = 'artifacts', asNode = asNode)
      suri <- sapply(AA, function(x) x$get_sample(form = "uri"))
      if (.self$lims$version == 'v1'){
         SS <- lapply(suri, function(x) {.self$lims$GET(x)})
      } else {
         SS <- .self$lims$batchretrieve(suri, rel = 'samples', asNode = asNode)
      }
      #names(SS) <- sapply(.self$node['placement'], xml_value)
      names(SS) <- names(puri)
      invisible(SS)
   })
   
###### methods above
###### functions below

#' Create a containers details node assembled from one or more container XML::xmlNode
#' See \url{http://genologics.com/files/permanent/API/latest/data_con.html#element_details}
#' 
#' @export
#' @param x one or more XML::xmlNode for container or ContainerRefClass objects
#' @param form character
#' @return a containers details XML:xmlNode node
create_containers_details <- function(x, form = c("retrieve", "update", "create") ){
   
   if (!is.list(x)) x <- list(x)
   
   if (inherits(x[[1]], "ContainerRefClass")){
      x <- lapply(x, "[[", "node")      
   }
   
   nm <- sapply(x, xml_name)
   if (!all(tolower(nm) == "container")) stop("create_container_details: input nodes must be of type container")
   
   XML::newXMLNode("details",
      namespace = "con",
      namespaceDefinitions = get_NSMAP()[c("ri",  "udf", "file", "art", "con")],
      .children = x)
} # create_containers_details


#' Create a container XML::xmlNode suitable for POSTing 
#' 
#' @export
#' @family Lims Container
#' @param name character container name (optional)
#' @param type_uri character uri of container type (required)
#' @param state character generally Empty but possibly Populated, Discarded, or Depleted
#' @return XML::xmlNode
create_container_node <- function(type_uri, name = NULL, state = 'Empty'){
   
      if (missing(type_uri)) stop("create_project_node type uri is required")
      
      nmsp <- 'con'
      
      kids <- list(
        XML::newXMLNode("type", attrs = list('uri' = type_uri[1])),
        XML::newXMLNode("state", state[1]))
      if (!is.null(name)) kids <- append(kids, XML::newXMLNode("name", name[1]))
      
      XML::newXMLNode('container',
         namespace = nmsp,
         namespaceDefinitions = get_NSMAP()[nmsp],
         .children = kids)
      
} # create_container_node
