# Lab.R

#' A Lab/Account representation that subclasses from NodeRefClass
#'
#' @field name character
#' @field website character
#' @field shipping_address character vector
#' @field billing_address character vector
#' @include Node.R
#' @export
LabRefClass <- setRefClass("LabRefClass",
    contains = "NodeRefClass",
    fields = list(
       name = 'character',
       website = 'character',
       shipping_address = 'character',
       billing_address = 'character'),
    methods = list(
        initialize = function(...){
              callSuper(...)
              .self$verbs <- c('GET', 'PUT', 'POST', 'BROWSE')
              .self$name = .self$get_name()
              .self$website = get_childvalue(.self$node, 'website')
              .self$shipping_address = .self$get_address("shipping")
              .self$billing_address = .self$get_address("billing")
            },
        update = function(...){
            callSuper(...)
            .self$name = .self$get_name()
            .self$website = get_childvalue(.self$node, 'website')
            .self$shipping_address = .self$get_address("shipping")
            .self$billing_address = .self$get_address("billing")
            },       
        show = function(prefix = ""){
            callSuper(prefix = prefix)
            cat(prefix, "  Lab name: ", .self$name, "\n", sep = "")
            if (nchar(.self$website) > 0) 
                cat(prefix, "  Lab website: ", .self$website, "\n", sep = "")
            },
        get_address = function(what = c('shipping', 'billing')[1]){
            x <- ""
            add <- .self$node[[paste0(tolower(what[1]), "-address")]]
            if (!is.null(add)){
                
                pp <- XML::xmlChildren(add)
                x <- sapply(pp, XML::xmlValue)
                names(x) <- names(pp)
            }
            return(x)
            }) # methods
    ) #setRefClass
    
##### Functions below


#' Create a lab/account XML::xmlNode suitable for POSTing 
#' 
#' @export
#' @param name character lab/account name (required)
#' @param website character website
#' @param shipping_address named character vector ala
#'  [street='123 Main', city='East Boothbay Harbor', etc.]
#' @param billing_address named character vector
#' @return XML::xmlNode

create_lab_node <- function(name, 
    shipping_address = NULL, billing_address = NULL, website = NULL){
    #lab:lab 
    #    xmlns:udf="http://genologics.com/ri/userdefined" 
    #    xmlns:ri="http://genologics.com/ri" 
    #    xmlns:lab="http://genologics.com/ri/lab"
    
    if (missing(name)) stop("create_lab_node: name is required")
    
    nmsp <- c("lab", "udf", "ri")
    kids  <- list(XML::newXMLNode("name", name[1]))
    if (!is.null(website)) 
        kids <-base::append(kids, XML::newXMLNode("website", website[1]))
    if (!is.null(shipping_address)){
        nm <- names(shipping_address)
        mailkids <- lapply(nm, 
            function(n){
                XML::newXMLNode(n, shipping_address[[n]])
            })
        mail <- XML::newXMLNode("shipping-address",
            .children = mailkids)
        kids <- base::append(kids, mail)
    }
    
    if (!is.null(billing_address)){
        nm <- names(billing_address)
        mailkids <- lapply(nm, 
            function(n){
                XML::newXMLNode(n, billing_address[[n]])
            })
        mail <- XML::newXMLNode("billing-address",
            .children = mailkids)
        kids <- base::append(kids, mail)
    }    
            
    XML::newXMLNode('lab',
        namespace = 'lab',
        namespaceDefinitions = get_NSMAP()[nmsp],
        .children = kids)
} # create_lab_node



      
