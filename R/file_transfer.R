# file_transfer.R


#' Download files using system cp
#'
#' This assumes permissions have been set up
#'
#' @family file_transfer
#' @export
#' @param url character
#' @param dest character destination filename, by default the basename of the URL
#' @param verbose logical, if TRUE then echo the command string
#' @param extra character extra params for cp, currently ""
#' @return integer with 0 for success
cp_download <- function(url, dest, 
    verbose = FALSE, extra = ""){
    
    p <- httr::parse_url(url[1])
    CMD <- if(nchar(extra) > 0) paste("cp", extra) else "cp"
    CMD <- paste(CMD, paste0("/",p[['path']]), dest[1])
    if (verbose) cat(CMD, "\n")
    system(CMD)
}




#' Test if rsync is installed on the host platform
#' Perhaps silly but symmetric with others.
#'
#' @family file_transfer
#' @export
#' @return logical, TRUE if rsync is present
has_rsync <- function() {
   basename(Sys.which("rsync")) == "rsync"
} 


#' Download a file using rsync - rsync and then rename
#'
#' This assumes that SSH public-key authentication is set up.
#'
#' @family file_transfer
#' @export
#' @param url character
#' @param dest character destination filename, by default the basename of the URL
#' @param username the username (required)
#' @param password the password (required)
#' @param verbose logical, if TRUE then echo the command string
#' @param extra character extra params for scp, currently "-q"
#' @return integer with 0 for success
rsync_download <- function(url, dest, 
   username = 'foo', password = 'bar',
   verbose = FALSE, extra = '-q'){
   
   #https://kb.iu.edu/d/agye
   #rsync [options] username1@source_host:directory1/filename1 username2@destination_host:directory2/filename2
   stopifnot(has_rsync())
   stopifnot(!missing(url))
   if (missing(dest)) dest <- file.path(getwd(),basename(url[1]))
   destdir <- dirname(dest)
   stopifnot(username != 'foo')
   stopifnot(password != 'bar')  
   
   p <- httr::parse_url(url)
   # first we copy the file to the destdir - it will hve the url basename
   CMD <- paste("rsync",
      extra,
      paste0(username, '@', p$hostname, ':/', p$path),
      destdir) 
   if (verbose) cat(CMD, "\n")  
   ok <- system(CMD)
   if (ok != 0) return(ok)
   
   # now we rename the file - ugggg
   CMD <- paste('mv -f', 
      file.path(destdir, basename(p$path)),
      dest)
   if (verbose) cat(CMD, "\n")
   system(CMD)
}


#' Upload a local file using rsync.  
#'
#' This assumes that SSH public-key authentication is set up.
#' 
#' @family file_transfer
#' @export
#' @param filename the fully qualified filename of file to upload
#' @param url the destination url
#' @param username the username (required)
#' @param password the password (required)
#' @param extra character extra params for scp, currently "-q"
#' @return integer 0 for success
rsync_upload <- function(filename, url, username = "foo", password = "bar", extra = '-q'){

   stopifnot(has_rsync())
   stopifnot(!missing(filename))
   stopifnot(!missing(url))
   stopifnot(file.exists(filename))
   stopifnot(username != 'foo')
   stopifnot(password != 'bar')
   
   p <- httr::parse_url(url)
   
   # make sure the path exist
   MKDIR <- paste('ssh',
      paste0(username,'@',p$server), 
      shQuote(paste('mkdir -p', paste0('/',dirname(p$path)))))
   ok <- system(MKDIR)
   if (ok != 0) {
      cat("unable to create destination path:", p$path, "\n")
      return(ok)
   }
   
   # now copy filename to the destdir
   CMD <- paste("rsync", extra,
      shQuote(filename[1]),
      paste0(username, '@', p$hostname, ':/', dirname(p$path)))
   if (verbose) cat(CMD, "\n")   
   ok <- system(CMD)
   if (ok != 0) return(ok)
   
   # now move the file - uggg.
   from <- file.path(dirname(p$path), shQuote(filename[1]))
   to <- p$path
   CMD <- paste('ssh',
      paste0(username,'@',p$server),
      shQuote(paste('mv -f', from, to)) )
   if (verbose) cat(CMD, "\n")   
   system(CMD)
}


################################################################################
#' Test if scp is installed on the host platform
#' Perhaps silly but symmetric with others.
#'
#' @family file_transfer
#' @export
#' @return logical, TRUE if scp is present
has_scp <- function() {
   basename(Sys.which("scp")) == "scp"
} 

#' Download a file using scp
#'
#' This assumes that SSH public-key authentication is set up.
#'
#' @family file_transfer
#' @export
#' @param url the url of the file
#' @param dest character destination filename, by default the basename of the URL
#' @param username the username (required)
#' @param password the password (required)
#' @param verbose logical, if TRUE then echo the command string
#' @param extra character extra params for scp, currently "-q"
#' @return integer with 0 for success
scp_download <- function(url, dest, username = 'foo', password = 'bar',
   verbose = FALSE, extra = '-q'){
   
   #https://kb.iu.edu/d/agye
   #scp [options] username1@source_host:directory1/filename1 username2@destination_host:directory2/filename2
   stopifnot(has_scp())
   stopifnot(!missing(url))
   if (missing(dest)) dest <- file.path(getwd(),basename(url[1]))
   stopifnot(username != 'foo')
   stopifnot(password != 'bar')  
   
   p <- httr::parse_url(url)
   
   CMD <- paste("scp",
      extra,
      paste0(username, '@', p$hostname, ':/', p$path),
      shQuote(dest[1]))
      
   if (verbose) cat(CMD, "\n")  
    
   system(CMD)
}


#' Upload a file using scp.  
#'
#' This assumes that SSH public-key authentication is set up.
#' 
#' @family file_transfer
#' @export
#' @param filename the fully qualified filename of file to upload
#' @param url the destination url
#' @param username the username (required)
#' @param password the password (required)
#' @return integer 0 for success
scp_upload <- function(filename, url, username = "foo", password = "bar"){

   stopifnot(has_scp())
   stopifnot(!missing(filename) && !missing(url))
   stopifnot(file.exists(filename))
   stopifnot(username != 'foo')
   stopifnot(password != 'bar')
   
   p <- httr::parse_url(url)
   
   MKDIR <- paste('ssh',
      paste0(username,'@',p$server), 
      shQuote(paste('mkdir -p', paste0('/',dirname(p$path)))))
   ok <- system(MKDIR)
   if (ok != 0) {
      cat("unable to create destination path:", p$path, "\n")
      return(ok)
   }
   
   CMD <- paste("scp",
      shQuote(filename[1]),
      paste0(username, '@', p$hostname, ':/', p$path))
      
   if (verbose) cat(CMD, "\n")   
   
   system(CMD)
}


################################################################################
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
      "--upload", url[1], shQuote(filename[1]))
      
   if (verbose) cat(CMD, "\n")   
   
   system(CMD)
}

#' Download a file using duck
#'
#' @family file_transfer
#' @export
#' @param url the url of the file
#' @param dest character destination filename, by default the basename of the URL
#' @param username the username (required)
#' @param password the password (required)
#' @param verbose logical, if TRUE then echo the command string
#' @param extra character extra params for duck, currently "--existing overwrite"
#' @return integer with 0 for success
duck_download <- function(url, dest, username = "foo", password = "bar",
   verbose = FALSE, extra = '--quiet --existing overwrite'){
   
   stopifnot(has_duck())
   stopifnot(!missing(url))
   if (missing(dest)) dest <- file.path(getwd(),basename(url[1]))
   stopifnot(username != 'foo')
   stopifnot(password != 'bar')

   CMD <- paste("duck",
      "--username", username[1],
      "--password", password[1],
      "--download", url[1], shQuote(dest[1]))
   if (!is.null(extra)) CMD <- paste(CMD, extra)
      
   system(CMD)
}