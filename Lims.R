# reference class for Lims

LimsRefClass <- setRefClass('Lims',
   fields = list(
      version = 'character',
      baseuri = 'character',
      username = 'character',
      password = 'character',
      handle = 'ANY')
)  


#### methods above
#### functions below

#' Read a configuration file
#' @param filename the name of the file
#' @return a name list with one element per section
#'  each section, in turn is a named list
lims_read_config = function(filename){
   #' Parse lines of text
   #' @param x the line(s) of text in the form of tag value pairs
   #' @return named character vector of tag1 = value1, tag2 = value2, ...
   parse_config_line <- function(x){
    pat <- '[=:]'
    ix <- regexpr(pat, x)
    r <- vector(mode = "character")
    for (i in seq_along(x)){
      if (ix[i] > 0 ){
        nm <- substring(x[i], 1, ix[i]-1)
        # strip leading spaces
        val <- sub("^ +", "", substring(x[i], ix[i] + 1, nchar(x[i]) ) )
        r[[nm]] <- val
      } # ix[i] > 0
    } # i-loop
    r
  }
 
   if (!file.exists(filename)) stop(paste("file must exist: ", filename))
   x <- scan(filename, what = character(), quiet = TRUE, sep = "\n", comment.char = "#")
   x <- x[!grepl("^#", x)]
   ix <- grep("^\\[", x)
   if (length(ix) == 0) stop("No [headers] found in config file - please check file")
   len <- nchar(x[ix])
   nm <- substring(x[ix], rep(2, length(len)), len-1)
   iy <- c(ix, length(x)+1)
   L <- list()
   for (i in seq_along(nm)) L[[nm[i]]] <- parse_config_line(x[(iy[i] + 1) : (iy[i+1]-1) ])
   invisible(L)
} # read_config

#' Retrive a configuration value
#' @param x the configuration list
#' @param section the name of the section
#' @param name the name of the tagged value, if missing then the section is returned
#' @param default the value to return if the tag doesn't exists (defaults to NULL)
#' @return the tagged value or section requested or the 'default' value if not found
lims_get_config <- function(x, section, name, default = NULL){
   if (nargs() < 2) stop("at least x and section are required")
   s <- x[[section[1]]]
   if (is.null(s)) return(default)
   if ( !(missing(name)) ) {
     if (name[1] %in% names(s)) return(s[[name[1]]])
   } else {
     return(s)
   }
   return(default)
} # get_config


Lims <- function(configfile){
   if (missing(configfile)) stop("configfile is required")
   if (!file.exists(configfile[1])) stop("configfile not found:", configfile[1])
   x <- try(lims_read_config(configfile[1]))
   if (inherits(x, "try-error")) stop("Error reading config file")
   
   X <- LimsRefClass$new()
   X$field("handle", NULL)
   X$field("version", get_config(x, "genologics", "VERSION", default = ""))
   X$field("baseuri", get_config(x, "genologics", "BASEURI", default = ""))
   X$field("username", get_config(x, "genologics", "USERNAME", default = ""))
   X$field("password", get_config(x, "genologics", "PASSWORD", default = ""))
   

}