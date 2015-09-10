# file_transfer.R

#' Test if duck is installed on the host platform
#' See \url{https://cyberduck.io/}
#'
#' @family file_transfer
#' @export
#' @return logical, TRUE if duck is present
has_duck <- function() {
   basename(Sys.which("duck")) == "duck"
}

#' Upload a file using duck
#' 
#' @family file_transfer
#' @export
#' @param filename the fully qualified filename of file to upload
#' @param url the destination url
#' @param username the username (required)
#' @param password the password (required)
#' @return integer 0 for success
duck_upload <- function(filename, url, username = "foo", password = "bar"){

   stopifnot(has_duck())
   stopifnot(!missing(filename) && !missing(url))
   stopifnot(file.exists(filename))
   stopifnot(username != 'foo')
   stopifnot(password != 'bar')
   
   CMD <- paste("duck",
      "--username", username[1],
      "--password", password[1],
      "--upload", url[1], filename[1])
      
   system(CMD)
}

#' Download a file using duck
#'
#' @family file_transfer
#' @export
#' @param url
#' @param dest character destination filename, by default the basename of the URL
#' @param username the username (required)
#' @param password the password (required)
#' @param verbose logical, if TRUE then echo the command string
#' @param extra character extra params for duck, currently "--existing overwrite"
#' @return integer with 0 for success
duck_download <- function(url, dest, username = "foo", password = "bar",
   verbose = FALSE, extra = '--existing overwrite'){
   
   stopifnot(has_duck())
   stopifnot(!missing(url))
   if (missing(dest)) dest <- file.path(getwd(),basename(url[1]))
   stopifnot(username != 'foo')
   stopifnot(password != 'bar')

   CMD <- paste("duck",
      "--username", username[1],
      "--password", password[1],
      "--download", url[1], dest[1])
   if (!is.null(extra)) CMD <- paste(CMD, extra)
      
   system(CMD)
}