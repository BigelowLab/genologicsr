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
