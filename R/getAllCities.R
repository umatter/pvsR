##' Get basic data on all cities
##' 
##' This function is essentially a  wrapper around Local.getCities().
##' @usage getAllCities()
##' @return A data frame with a row for each city and columns with the following variables describing the city:\cr cities.city*.localId,\cr cities.city*.name,\cr cities.city*.url.
##' @references http://api.votesmart.org/docs/Local.html\cr
##' Use State.getStateIDs() to get a list of state IDs.\cr
##' See also: Matter U, Stutzer A (2015) pvsR: An Open Source Interface to Big Data on the American Political Sphere. PLoS ONE 10(7): e0130501. doi: 10.1371/journal.pone.0130501
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @examples
##' # First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
##' \dontrun{pvs.key <- "yourkey"}
##' # get a list of all cities
##' \dontrun{cities <- getAllCities()}
##' \dontrun{head(cities)}
##' @export



getAllCities <-
  function() {
    
    states <- State.getStateIDs()
    #nocounties <- c("NA", "PR", "GU", "AS", "VI", "DC")
    #states <- states[!(states$stateId %in% nocounties),]
    Local.getCities(states$stateId)  
  }