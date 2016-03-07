# Containertype.R

#' A Containertype representation that subclasses from NodeRefClass
#' 
#' @family Node
#' @field name character, name if any
#' @field is_tube logical, type if any
#' @field xdim, list [is_alpha = TRUE/FALSE, offset = 0/1, size = n]
#' @field ydim, list [is_alpha = TRUE/FALSE, offset = 0/1, size = n]
#' @include Node.R
#' @export
ContainerTypeRefClass <- setRefClass("ContainerTypeRefClass",
   contains = "NodeRefClass",
   fields = list(
      name = 'character',
      is_tube = 'logical',
      xdim = 'list',
      ydim = 'list'),
   methods = list(
      show = function(prefix = ""){
         callSuper(prefix = prefix)
         cat(prefix, "  Container type name: ", .self$name, "\n", sep = "")
         cat(prefix, "  Container type is_tube: ", .self$is_tube, "\n", sep = "")
         cat(prefix, "  Container type width: ", .self$xdim[["size"]], "\n", sep = "")
         cat(prefix, "  Container type height: ", .self$ydim[["size"]], "\n", sep = "")
      },
      get_dim = function(what = 'x'){
         what = paste0(tolower(what[1]), "-dimension")
         x <- .self$node[[what]]
         list(
            is_alpha = xml_value(x[['is-alpha']]) == "true",
            offset = as.numeric(xml_value(x[['offset']])),
            size = as.numeric(xml_value(x[['size']])))
      },
      test_if_tube = function(){
         if (.self$lims$version == 'v1'){
            istube <- (.self$xdim[['size']] * .self$ydim[['size']]) == 1
         } else {
            istube <- xml_value(.self$node[['is-tube']]) == 'true'
         }
         return(istube)
      },      
      initialize = function(...){
         callSuper(...)
         .self$verbs <- c("GET", "BROWSE")
         .self$name = xml_atts(.self$node)[['name']]
         .self$xdim = .self$get_dim(what="x")
         .self$ydim = .self$get_dim(what="y")
         .self$is_tube = .self$test_if_tube()
      }
      ) # methods
      
   ) #setRefClass

