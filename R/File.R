#' File.R

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
               .self$attached_to <- xmlValue(.self$node[['attached-to']])
            if ('content-location' %in% nm)
               .self$content_location <- xmlValue(.self$node[['content-location']]) 
            if ('original-location' %in% nm)
               .self$original_location <- xmlValue(.self$node[['original-location']]) 
            if ('is-published' %in% nm)
               .self$is_published <- xmlValue(.self$node[['is-published']]) 
         }
      })
   )

#' Show
#' 
#' @family Node File
NULL
FileRefClass$methods(
   show = function(prefix = ""){
      callSuper(prefix = prefix)
      cat(prefix, "  File attached_to: ", .self$attached_to, "\n", sep = "")
      cat(prefix, "  File content_location: ", .self$content_location, "\n", sep = "")
      cat(prefix, "  File original_location: ", .self$original_location, "\n", sep = "")
      cat(prefix, "  File is_published: ", .self$is_published, "\n", sep = "")
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
   namespace = 'file', ...){
   nsr <- get_NSMAP()[namespace[1]]
   newXMLNode(namespace[1],
      namespace = namespace[1],
      namespaceDefinitions = nsr,,
      .children = list(
         newXMLNode("attached-to", attached_to),
         newXMLNode("original-location", original_location)) )
}
