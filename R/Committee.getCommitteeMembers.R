##' Get a list of members of a committee
##' 
##' This function is a wrapper for the Committee.getCommitteeMembers() method of the PVS API Committee class which returns a list of members of a committee. The function sends a request with this method to the PVS API for all committee IDs given as a function input, extracts the XML values from the returned XML file(s) and returns them arranged in one data frame.
##' @usage Committee.getCommitteeMembers(committeeId)
##' @param committeeId a character string or list of character strings with the committee ID(s) (see references for details)
##' @return A data frame with a row for each committee member and columns with the following variables describing the committee member:\cr committeeMembers.committee.committeeId,\cr committeeMembers.committee.parentId,\cr committeeMembers.committee.name,\cr committeeMembers.member*.candidateId,\cr committeeMembers.member*.title,\cr committeeMembers.member*.firstName,\cr committeeMembers.member*.middleName,\cr committeeMembers.member*.lastName,\cr committeeMembers.member*.suffix,\cr committeeMembers.member*.party,\cr committeeMembers.member*.position.
##' @references http://api.votesmart.org/docs/Committee.html\cr
##' Use CandidateBio.getBio(), Committee.getCommitteesByTypeState() or Votes.getBill() to get committee ID(s).\cr
##' See also: Matter U, Stutzer A (2015) pvsR: An Open Source Interface to Big Data on the American Political Sphere. PLoS ONE 10(7): e0130501. doi: 10.1371/journal.pone.0130501
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @examples
##' # First, make sure your personal PVS API key is saved as an option
##' # (options("pvs.key" = "yourkey")) or in the pvs.key variable:
##' \dontrun{pvs.key <- "yourkey"}
##' # get a list of members of certain committees
##' \dontrun{comember <- Committee.getCommitteeMembers(1)}
##' \dontrun{comember}
##' @export


Committee.getCommitteeMembers <-
	function (committeeId) {

		#internal function:
		Committee.getCommitteeMembers.basic <- 
			function(.committeeId) {
    		pvs.key <- getPVS_key()
				request <-  "Committee.getCommitteeMembers?"
				inputs  <-  paste("&committeeId=",.committeeId, sep="")
				pvs.url <- paste("http://api.votesmart.org/",request,"key=",pvs.key,inputs,sep="") #generate url for request
				doc <- xmlTreeParse(pvs.url)
				a <- xmlRoot(doc)
				
				if (length(a)==1 && names(a[1])=="errorMessage") {
					warning(gsub(pattern="&", replacement=" ", x=paste("No data available for: ", inputs,". The corresponding rows in the data frame are filled with NAs.", sep=""), fixed=TRUE), call.=FALSE)
					output.df <- data.frame("committeeId"=as.character(.committeeId), stringsAsFactors = FALSE)
				
					} else {
					items <- getNodeSet(a, path="//member")
					output.items <- lapply(items, function(x) data.frame(t(unlist(xmlSApply(x, xmlValue))), row.names=NULL, stringsAsFactors = FALSE))
					output.items.df <- bind_rows(output.items)
					output.base.df <- data.frame(t(xmlSApply(a[[2]], xmlValue)), stringsAsFactors = FALSE)
					output.df <- merge(output.base.df, output.items.df)
				}
				return(output.df)
			}
		
		# Main function 
		output.list <- lapply(committeeId, FUN= function (b) {
			Committee.getCommitteeMembers.basic(.committeeId=b)
		}
		)
		if (length(output.list)>1) {
			output <- bind_rows(output.list)
		} else {
			output <- as.tbl(output.list[[1]])
		}
		
		return(output)
	}

