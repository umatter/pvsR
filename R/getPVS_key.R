##' Get user's Project Vote Smart API key.
##' 
##' This function checks for a "pvs.key" option. If the option exists, its 
##' stored value is presumed to be the API key. If the option doesn't exist,
##' the function checks the global environment for a \code{pvs.key} variable 
##' and uses it as the API key. The function returns the key as a character
##' string.
##'
##' @author John Bullock <john@johnbullock.org>
##'
##' @examples
##' \dontrun{options("pvs.key" = "yourkey")} 
##' \dontrun{pvs.key <- "yourkey"}



# Look first for the "pvs.key" option, then for a "pvs.key" variable in the 
# global environment.  
getPVS_key <- function () {
  pvs.key <- getOption("pvs.key")

  if (is.null(pvs.key)) {
    if (exists("pvs.key", envir = .GlobalEnv)) {
      pvs.key <- get('pvs.key', envir = .GlobalEnv)
    }
    else {
      stop("pvs.key couldn't be found")
    }
  }
  
  pvs.key
}
