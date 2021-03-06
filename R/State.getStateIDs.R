##' Get a list of states and their IDs.
##' 
##' This function is a wrapper for the State.getStateIDs() method of the PVS API State class which returns a simple state ID and name list for mapping IDs to state names. 
##' @usage State.getStateIDs()
##' @return A data frame with a row for each state:\cr
##' statelist.list.state*.stateId,\cr
##' statelist.list.state*.name
##' @references http://api.votesmart.org/docs/State.html\cr
##' See also: Matter U, Stutzer A (2015) pvsR: An Open Source Interface to Big Data on the American Political Sphere. PLoS ONE 10(7): e0130501. doi: 10.1371/journal.pone.0130501
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @examples
##' # First, make sure your personal PVS API key is saved as an option 
##' # (options("pvs.key" = "yourkey")) or in the pvs.key variable:
##' \dontrun{pvs.key <- "yourkey"}
##' # get a list of states with their IDs
##' \dontrun{stateIDs <- State.getStateIDs()}
##' \dontrun{stateIDs}
##' @export



State.getStateIDs <-
	function () {
		pvs.key <- getPVS_key()
  
    states <- xmlTreeParse(paste("http://api.votesmart.org/State.getStateIDs?key=", pvs.key, sep=""), useInternalNodes = TRUE)
		states <- xmlRoot(states)
		states <- states[["list"]]
		
		statelist <- xmlSApply(states, function(x) xmlSApply(x, xmlValue))
		states.df <- as.tbl(as.data.frame(t(statelist),row.names=FALSE))
		
		return(states.df)
	}
		
		

