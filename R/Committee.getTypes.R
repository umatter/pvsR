##' Get the committee types (house, senate, joint, etc.)
##' 
##' This function is a wrapper for the Committee.getTypes() method of the PVS API Committee class which returns the existing committee types.
##' @usage Committee.getTypes()
##' @return A data frame with a row for each committee type and columns with the following variables describing the committee type:\cr committeeTypes.type*.committeeTypeId,\cr committeeTypes.type*.name.
##' @references http://api.votesmart.org/docs/Committee.html\cr
##' See also: Matter U, Stutzer A (2015) pvsR: An Open Source Interface to Big Data on the American Political Sphere. PLoS ONE 10(7): e0130501. doi: 10.1371/journal.pone.0130501
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @examples
##' # First, make sure your personal PVS API key is saved as an option
##' # (options("pvs.key" = "yourkey")) or in the pvs.key variable:
##' \dontrun{pvs.key <- "yourkey"}
##' # get the committee types
##' \dontrun{comtypes <- Committee.getTypes()}
##' \dontrun{comtypes}
##' @export



Committee.getTypes <-
	function() {
		request <-  "Committee.getTypes?"
		inputs  <-  ""
		url.base <- "http://api.votesmart.org/"
		pvs.key <- getPVS_key()		
		pvs.url <- paste(url.base,request,"key=",pvs.key,inputs,sep="") #generate url for request
		output <- t(xmlSApply(removeChildren(xmlRoot(xmlTreeParse(pvs.url,useInternalNodes=TRUE)),kids=1), function(x) xmlSApply(x, xmlValue)))
		output <- as.tbl(data.frame(output, row.names=NULL, stringsAsFactors = FALSE))
		
		return(output)
	}

