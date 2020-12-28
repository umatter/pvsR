##' Get a list of bills according to sponsor(candidate) and category
##' 
##' This function is a wrapper for the Votes.getBillsBySponsorCategory() method of the PVS API Votes class which grabs a list of bills that fit the sponsor's candidateId and category. The function sends a request with this method to the PVS API for all candidate and category IDs given as a function input, extracts the XML values from the returned XML file(s) and returns them arranged in one data frame.
##' @usage Votes.getBillsBySponsorCategory(categoryId, candidateId)
##' @param categoryId a character string or list of character strings with the category ID(s) (see references for details)
##' @param candidateId a character string or list of character strings with the candidate ID(s) (see references for details)
##' @return A data frame with a row for each bill and columns with the following variables describing the bill:\cr bills.bill*.billId,\cr bills.bill*.billNumber,\cr bills.bill*.title,\cr bills.bill*.type.
##' @references http://api.votesmart.org/docs/Votes.html\cr
##' Use Votes.getCategories() to get a list of category IDs.\cr
##' Use Candidates.getByOfficeState(), Candidates.getByOfficeTypeState(), Candidates.getByLastname(), Candidates.getByLevenshtein(), Candidates.getByElection(), Candidates.getByDistrict() or Candidates.getByZip() to get a list of candidate IDs.\cr
##' See also: Matter U, Stutzer A (2015) pvsR: An Open Source Interface to Big Data on the American Political Sphere. PLoS ONE 10(7): e0130501. doi: 10.1371/journal.pone.0130501
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @examples
##' # First, make sure your personal PVS API key is saved as an option
##' # (options("pvs.key" = "yourkey")) or in the pvs.key variable:
##' \dontrun{pvs.key <- "yourkey"}
##' # get a list of bills for a certain candidate and category
##' \dontrun{bills <- Votes.getBillsBySponsorCategory(as.list(1:50),107800)}
##' \dontrun{bills}
##' @export


Votes.getBillsBySponsorCategory <-
	function (categoryId, candidateId) {

		# internal function
		Votes.getBillsBySponsorCategory.basic <- 
			function (.categoryId, .candidateId) {
				request <-  "Votes.getBillsBySponsorCategory?"
				inputs  <-  paste("&categoryId=",.categoryId,"&candidateId=",.candidateId,sep="")
				output  <-  pvsRequest(request,inputs)
				output$categoryId <- .categoryId
				output$candidateId <- .candidateId
				
				return(output)
			}

		# Main function  
		output.list <- lapply(categoryId, FUN= function (y) {
			lapply(candidateId, FUN= function (s) {
				Votes.getBillsBySponsorCategory.basic(.categoryId=y, .candidateId=s)
			}
			)
		}
		)
		output.list <- redlist(output.list)
		output <- bind_rows(output.list)

		return(output)
	}
