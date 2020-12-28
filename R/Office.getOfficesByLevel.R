##' Get offices tracked according to level
##' 
##' This function is a wrapper for the Office.getOfficesByLevel() method of the PVS API Office class which grabs a list of offices Project Vote Smart keeps track of according to their level. The function sends a request with this method to the PVS API for all level IDs given as a function input, extracts the XML values from the returned XML file(s) and returns them arranged in one data frame.
##' @usage Office.getOfficesByLevel(levelId)
##' @param levelId a character string or list of character strings with the level ID(s) (see references for details)
##' @return A data frame with a row for each office and columns with the following variables describing the office:\cr offices.office*.officeId,\cr offices.office*.officeTypeId,\cr offices.office*.officeLevelId,\cr offices.office*.officeBranchId,\cr offices.office*.name,\cr offices.office*.title,\cr offices.office*.shortTitle.
##' @references http://api.votesmart.org/docs/Office.html\cr
##' Use Office.getLevels() to get level ID(s).\cr
##' See also: Matter U, Stutzer A (2015) pvsR: An Open Source Interface to Big Data on the American Political Sphere. PLoS ONE 10(7): e0130501. doi: 10.1371/journal.pone.0130501
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @examples
##' # First, make sure your personal PVS API key is saved as an option
##' # (options("pvs.key" = "yourkey")) or in the pvs.key variable:
##' \dontrun{pvs.key <- "yourkey"}
##' # get offices tracked for all levels
##' \dontrun{offices <- Office.getOfficesByLevel(list("F","S","L"))}
##' \dontrun{head(offices)}

##' @export



Office.getOfficesByLevel <-
	function (levelId) {

		# internal function
		Office.getOfficesByLevel.basic <- 
			function (.levelId) {
			
			request <-  "Office.getOfficesByLevel?"
			inputs  <-  paste("&levelId=",.levelId,sep="")
			output  <-  pvsRequest4(request,inputs)
			
			return(output)
		}

		# Main function  
		output.list <- lapply(levelId, FUN= function (s) {
			Office.getOfficesByLevel.basic(.levelId=s)
		}
		)

		output.list <- redlist(output.list)
		output <- bind_rows(output.list)

		return(output)
	}
