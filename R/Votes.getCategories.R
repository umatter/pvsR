##' Get a list of categories that contain released bills according to year and state
##' 
##' This function is a wrapper for the Votes.getCategories() method of the PVS API Votes class which dumps categories that contain released bills according to year and state. The function sends a request with this method to the PVS API for all years and state IDs given as a function input, extracts the XML values from the returned XML file(s) and returns them arranged in one data frame.
##' @usage Votes.getCategories(year, stateId="NA")
##' @param year a character string or list of character strings with the year(s)
##' @param stateId (optional) a character string or list of character strings with the state ID(s) (default: "NA", for national) (see references for details)
##' @return A data frame with a row for each category and columns with the following variables describing the category:\cr categories.category*.categoryId,\cr categories.category*.name.
##' @references http://api.votesmart.org/docs/Votes.html\cr
##' Use State.getStateIDs() to get a list of state IDs.\cr
##' See also: Matter U, Stutzer A (2015) pvsR: An Open Source Interface to Big Data on the American Political Sphere. PLoS ONE 10(7): e0130501. doi: 10.1371/journal.pone.0130501
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @examples
##' # First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
##' \dontrun{pvs.key <- "yourkey"}
##' # get a list of categories
##' \dontrun{categories <- Votes.getCategories(2012)}
##' \dontrun{categories}

##' @export


Votes.getCategories <-
	function (year, stateId="NA") {

		# internal function
		Votes.getCategories.basic <- 
			function (.year, .stateId) {

				request <-  "Votes.getCategories?"
				inputs  <-  paste("&year=",.year,"&stateId=",.stateId,sep="")
				output  <-  pvsRequest(request,inputs)
				output$year <-.year
				output$stateId <- .stateId
				return(output)
			}
		
		#Main function
		output.list <- lapply(year, FUN= function (y) {
			lapply(stateId, FUN= function (s) {
				Votes.getCategories.basic(.year=y, .stateId=s)
			}
			)
		}
		)
		output.list <- redlist(output.list)
		output <- bind_rows(output.list)
		
		return(output)
		
		}
