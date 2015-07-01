# Artifact.R

ArtifactRefClass <- setRefClass("ArtifactRefClass",
   contains = "NodeRefClass",
      fields = list(
      name = 'character',
      type = 'character'),
   methods = list(
      initialize = function(...){
         callSuper(...)
         .self$name = .self$get_name()
         .self$type = .self$get_type()
      })
   )

Artifact <- getRefClass("ArtifactRefClass")

#' Show
#' 
#' @family Node Artifact
NULL
ArtifactRefClass$methods(
   show = function(prefix = ""){
      callSuper(prefix = prefix)
      cat(prefix, "  Artifact name: ", .self$name, "\n", sep = "")
      cat(prefix, "  Artifact type: ", .self$type, "\n", sep = "")
      cat(prefix, "  Artifact parent-process: ", .self$get_parent_process(form = "uri"), "\n", sep = "")
      cat(prefix, "  Artifact sample: ", .self$get_sample(form = "uri"), "\n", sep = "")
      cat(prefix, "  Artifact location: ", .self$location(), "\n", sep = "")
      cat(prefix, "  Artifact qc-flag: ", .self$qc_flag(), "\n", sep = "")
      cat(prefix, "  Artifact working-flag: ", .self$working_flag(), "\n", sep = "")
   })  

#' Get a pretty string representing the location
#' 
#' @name ArtifactRefClass_location
#' @return charcater of containerLimsid_location
NULL
ArtifactRefClass$methods(
   location = function(){
      if (.self$has_child("location")){
         con <- XML::xmlAttrs(.self$node[['location']][['container']])[['limsid']]
         plc <- XML::xmlValue(.self$node[['location']][['value']])
         r <- paste(basename(con), plc, sep = "_")
      } else {
         r <- ""
      }
      r
   })

#' Get the qc flag
#' 
#' @name ArtifactRefClass_qc_flag
#' @return charcater of containerLimsid_location
NULL
ArtifactRefClass$methods(
   qc_flag = function(){
      get_childvalue(.self$node, "qc-flag")
   })
   
#' Get the working flag
#' 
#' @name ArtifactRefClass_working_flag
#' @return charcater of containerLimsid_location
NULL
ArtifactRefClass$methods(
   working_flag = function(){
      get_childvalue(.self$node, "working-flag")
   })
   
 
#' Get the parent_process assocated with this artifact
#'
#' @family Artifact
#' @name ArtifactRefClass_get_parent_process
#' @param form character flag of type to return "xmlNode" or "uri"
#' @return XML::xmlNode (or NULL) or the uri (or "")
ArtifactRefClass$methods(
   get_parent_process = function(form = c("xmlNode", "uri")){
      if (!.self$has_child("parent-process")){
         x <- switch(tolower(form),
            "uri" = "",
            NULL)
         return(x)
      }
      thisuri <- trimuri(xmlAttrs(.self$node[["parent-process"]])[['uri']])
      if (tolower(form == "uri")){
         x <- thisuri
      } else {
         x <- .self$lims$GET(thisuri, asNode = TRUE)
      }
      invisible(x)
   })  
     
#' Get the sample  associated with this artifact
#'
#' @family Artifact
#' @name ArtifactRefClass_get_sample
#' @param form character flag of type to return "xmlNode" or "uri"
#' @return XML::xmlNode (or NULL) or the uri (or "")
ArtifactRefClass$methods(
   get_sample = function(form = c("xmlNode", "uri")){
      if (!.self$has_child("sample")){
         x <- switch(tolower(form[1]),
            "uri" = "",
            NULL)
         return(x)
      }
      thisuri <- trimuri(xmlAttrs(.self$node[["sample"]])[['uri']])
      if (tolower(form == "uri")){
         x <- thisuri
      } else {
         x <- .self$lims$GET(thisuri, asNode = TRUE)
      }
      invisible(x)
   })  