##' Get cities in a state
##' 
##' This function is a wrapper for the Local.getCities() method of the PVS API Local class which returns a list of cities in a state. The function sends a request with this method to the PVS API for all state IDs given as a function input, extracts the XML values from the returned XML file(s) and returns them arranged in one data frame.
##' @usage Local.getCities(stateId)
##' @param stateId a character string or list of character strings with the state ID(s) (see references for details)
##' @return A data frame with a row for each city and columns with the following variables describing the city:\cr cities.city*.localId,\cr cities.city*.name,\cr cities.city*.url.
##' @references http://api.votesmart.org/docs/Local.html\cr
##' Use State.getStateIDs() to get a list of state IDs.\cr
##' See also: Matter U, Stutzer A (2015) pvsR: An Open Source Interface to Big Data on the American Political Sphere. PLoS ONE 10(7): e0130501. doi: 10.1371/journal.pone.0130501
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @examples
##' # First, make sure your personal PVS API key is saved as an option
##' # (options("pvs.key" = "yourkey")) or in the pvs.key variable:
##' \dontrun{pvs.key <- "yourkey"}
##' # get a list of cities of a certain state
##' \dontrun{cities <- Local.getCities(list("NY","FL"))}
##' \dontrun{head(cities)}
##' @export




Local.getCities <-
	function (stateId) {

		# internal function
		Local.getCities.basic <- 
			function (.stateId) {
				request <-  "Local.getCities?"
				inputs  <-  paste("&stateId=",.stateId, sep="")
				output  <-  pvsRequest4(request,inputs)
				output$stateId <-.stateId
				
				return(output)
			}

		# Main function
		output.list <- lapply(stateId, FUN= function (b) {
			Local.getCities.basic(.stateId=b)
		}
		)
		
		output.list <- redlist(output.list)
		output <- bind_rows(output.list)
		
		if ("data.frame" %in% class(output)) {
			return(output)
		}
	}
