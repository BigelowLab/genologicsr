#' Make a remote directory specified by a URL using 
#'  \code{cp}, \code{scp} or \code{curl}.
#'
#' \code{curl} builds paths as needed (recursively) so if you select
#' \code{curl} we return success without doing anything.
#'
#' @family file_transfer
#' @export
#' @param uri character
#' @param username the username (required)
#' @param password the password (required for curl)
#' @param verbose logical, if TRUE then echo the command string
#' @param use character one of 'cp', 'scp' or 'curl'
#' @return integer with 0 for success
make_remote_directory <- function(uri, 
  username = 'foo', password = 'bar',
  use = c("cp", "scp", "curl")[2], 
  verbose = FALSE){
     
  puri <- httr::parse_url(uri)
  
  r = switch(tolower(use[1]),
      "scp" = {
        if (remote_directory_exists(uri, username = username, use = 'scp', verbose = verbose) != 0){
          r = 0
        } else {
          cmd <- paste('ssh',
             paste0(username,'@',puri[['hostname']]), 
             shQuote(paste('mkdir -p', paste0("/", puri[['path']] ) )) )
          if (verbose) cat("[make_remote_directory]", cmd, "\n")
          r = system(cmd)
        }
        r  
      }, 
      "curl" = 0,
      "cp" = {
        if (remote_directory_exists(uri, username = username, use = 'scp', verbose = verbose) != 0 ){
          r = 0
        } else {
          cmd = paste('mkdir -p', paste0("/", puri[['path']] ) )
          if (verbose) cat("[make_remote_directory]", cmd, "\n")
          r = system(cmd)
        }
        r
      },
      stop("[make_remote_directory] 'use' argument not known:", use))
      
  if(verbose){
    msg = sprintf("[make_remote_directory] remote directory %s found or created: %s",
      if (r == 0) "was" else "was not", uri)
    cat(msg, "\n")
  }
  r
}

#' Test if a remote directory exists
#'
#' see https://stackoverflow.com/questions/15927911/how-to-check-if-dir-exists-over-ssh-and-return-results-to-host-machine
#' @family file_transfer
#' @export
#' @param uri character
#' @param username the username (required)
#' @param password the password 
#' @param verbose logical, if TRUE then echo the command string
#' @param use character one of 'cp', 'scp' or 'curl'
#' @return integer with 0 for success
remote_directory_exists <- function(uri,
  username = 'foo', password = 'bar',
  use = c("cp", "scp", "curl")[2], 
  verbose = FALSE){
    
  puri <- httr::parse_url(uri)
  
  r = switch(tolower(use[1]),
      "scp" = {
        cmd <- paste('ssh',
           paste0(username,'@',puri[['hostname']]), 
           shQuote(paste('[ -d', paste0("/", puri[['path']] ), "]" )) )
        if (verbose) cat("[remote_directory_exists]", cmd, "\n")
        system(cmd)
      }, 
      "curl" = 0,
      "cp" = {
        cmd = paste('[ -d ', paste0("/", puri[['path']] ), ']' )
        if (verbose) cat("[remote_directory_exists]", cmd, "\n")
        system(cmd)
      },
      stop("[remote_directory_exists] 'use' argument not known:", use))
      
  if(verbose){
    msg = sprintf("[remote_directory_exists] remote directory %s found: %s",
                  if (r == 0) "was" else "was not", uri)
    cat(msg, "\n")
  }
  
  r
}


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
   #stopifnot(username != 'foo')
   #stopifnot(password != 'bar')  
   
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
#' @param verbose logical, if TRUE then echo the command string
#' @return integer 0 for success
rsync_upload <- function(filename, url, username = "foo", password = "bar", extra = '-q',
    verbose = FALSE){

   stopifnot(has_rsync())
   stopifnot(!missing(filename))
   stopifnot(!missing(url))
   stopifnot(file.exists(filename))
   #stopifnot(username != 'foo')
   #stopifnot(password != 'bar')
   
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
   #stopifnot(username != 'foo')
   #stopifnot(password != 'bar')  
   
   p <- httr::parse_url(url)
   
   CMD <- paste("scp",
      extra,
      paste0(username, '@', p$hostname, ':/', p$path),
      shQuote(dest[1]))
      
   if (verbose) cat("[scp_download]", CMD, "\n")  
    
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
#' @param verbose logical, if TRUE then echo the command string
#' @return integer 0 for success
scp_upload <- function(filename, url, username = "foo", password = "bar",
    verbose = FALSE){

   stopifnot(has_scp())
   stopifnot(!missing(filename) && !missing(url))
   stopifnot(file.exists(filename))
   #stopifnot(username != 'foo')
   #stopifnot(password != 'bar')
   
   if (verbose) cat("[scp_upload]", url, "\n")
   
   okdir = make_remote_directory(dirname(url),
                                 username = username, password = password,
                                 use = 'scp', verbose = verbose)
   if (okdir != 0) {
      cat("[scp_upload] unable to create destination path:", dirname(url), "\n")
      return(okdir)
   }
   
   p <- httr::parse_url(url)
   
   # MKDIR <- paste('ssh',
   #    paste0(username,'@',p$server), 
   #    shQuote(paste('mkdir -p', paste0('/',dirname(p$path)))))
   # ok <- system(MKDIR)
   # if (ok != 0) {
   #    cat("unable to create destination path:", p$path, "\n")
   #    return(ok)
   # }
   
   CMD <- paste("scp",
      shQuote(filename[1]),
      paste0(username, '@', p$hostname, ':/', p$path))
      
   r = system(CMD)
   if (verbose) {
     cat(CMD, "\n")  
     cat(sprintf("[scp_upload] scp_upload %s successful", if (r == 0) "was" else "was not"), "\n")
   }
   return(r)
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
#' @param verbose logical if TRUE echo the command
#' @return integer 0 for success
duck_upload <- function(filename, url, username = "foo", password = "bar", 
    verbose = FALSE){

   stopifnot(has_duck())
   stopifnot(!missing(filename) && !missing(url))
   stopifnot(file.exists(filename))
   #stopifnot(username != 'foo')
   #stopifnot(password != 'bar')
   
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
   #stopifnot(username != 'foo')
   #stopifnot(password != 'bar')

   CMD <- paste("duck",
      "--username", username[1],
      "--password", password[1],
      "--download", url[1], shQuote(dest[1]))
   if (!is.null(extra)) CMD <- paste(CMD, extra)
      
   system(CMD)
}