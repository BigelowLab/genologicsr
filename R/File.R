#' File.R

#' A File representation that subclasses from NodeRefClass
#' 
#' @family Node
#' @field attached_to character, uri of Artifact attached to
#' @field content_location character, location of file within filestore
#' @field original_location character, the file path to original location of file
#' @field is_published character, indicates if the file has been published
#' @include Node.R
#' @export
FileRefClass <- setRefClass("FileRefClass",
   contains = "NodeRefClass",
   fields = list(
      attached_to = 'character',
      content_location = 'character',
      original_location = 'character',
      is_published = 'character'),
   methods = list(
      initialize = function(...){
         callSuper(...)
         if (is_xmlNode(.self$node)){
            nm <- names(.self$node)
            if ('attached-to' %in% nm)
               .self$attached_to <- XML::xmlValue(.self$node[['attached-to']])
            if ('content-location' %in% nm)
               .self$content_location <- XML::xmlValue(.self$node[['content-location']]) 
            if ('original-location' %in% nm)
               .self$original_location <- XML::xmlValue(.self$node[['original-location']]) 
            if ('is-published' %in% nm)
               .self$is_published <- XML::xmlValue(.self$node[['is-published']]) 
         }
      })
   )

File <- getRefClass("FileRefClass")

#' Show
#' 
#' @family Node File
#' @name FileRefClass_show
NULL
FileRefClass$methods(
   show = function(prefix = ""){
      callSuper(prefix = prefix)
      cat(prefix, "  File attached_to: ", .self$attached_to, "\n", sep = "")
      cat(prefix, "  File content_location: ", .self$content_location, "\n", sep = "")
      cat(prefix, "  File original_location: ", .self$original_location, "\n", sep = "")
      cat(prefix, "  File is_published: ", .self$is_published, "\n", sep = "")
   })  


#' Download the file identified in 'content-location'
#' 
#' Download tools include 
#' \itemize{
#'    \item{duck}{From Cyberduck \url{https://duck.sh/}}
#'    \item{scp}{not implemented yet}
#' }
#' @family File
#' @name FileRefClass_download
#' @param dest filename for destination, by default the basename of the 'content_location'
#' @param use character indicating download tool to use.  Currently just 'duck'
#' @param ... further arguments for the download tool
#' @return numeric code where 0 means success
NULL
FileRefClass$methods(
   download = function(dest = NULL, use = c("duck", "scp")[1], ...){
      
      if (nchar(.self$content_location)) stop("FileRefClass$download: Node is not populated")
      if (nchar(dest[1]) == 0) dest <- basename(.self$content_location)
      up <- .self$lims$userpwd()
      switch(tolower(use[1]),
         'duck' = duck_download(.self$content_location[1], dest[1],
            username = up[1], password = up[2]),
         function(){ "Download tool not known" ; return(1) })
   })

   

#' Create an unresolved file node
#' 
#' @export
#' @family Lims File
#' @param attached_to character uri of the artifact to attach the file to
#' @param original_location character, the fully qualified path of the original file
#' @param namespace character the namespace for the resource
#' @return XML::xmlNode
create_file_node <- function(attached_to = "", original_location = "",
   namespace = 'file'){
   nsr <- get_NSMAP()[namespace[1]]
   XML::newXMLNode(namespace[1],
      namespace = namespace[1],
      namespaceDefinitions = nsr,
      .children = list(
         XML::newXMLNode("attached-to", attached_to),
         XML::newXMLNode("original-location", original_location)) )
}
