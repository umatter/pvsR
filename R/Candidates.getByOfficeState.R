##' Get a list of candidates according to office and state representation
##' 
##' This function is a wrapper for the Candidates.getByOfficeState() method of the PVS API Candidate class which grabs a list of candidates according to office and state representation. The function sends a request with this method to the PVS API for all state IDs, office IDs and election years given as a function input, extracts the XML values from the returned XML file(s) and returns them arranged in one data frame.
##' @usage Candidates.getByOfficeState(stateId="NA", officeId, electionYear=NULL, all=FALSE)
##' @param stateId (optional) a character string or list of character strings with the state ID(s) (default: "NA", for national) (see references for details)
##' @param officeId a character string or list of character strings with the office ID(s) (see references for details)
##' @param electionYear (optional) a character string or list of character strings with the election year(s) (default: >= current year)
##' @param all a logical indicator; if TRUE data on all possible combinations of the input variables are returned, if FALSE (default) only the exact combinations of them (see example)
##' @return A data frame with a row for each candidate and year and columns with the following variables describing the candidate:\cr candidateList.candidate*.candidateId,\cr candidateList.candidate*.firstName,\cr candidateList.candidate*.nickName,\cr candidateList.candidate*.middleName,\cr candidateList.candidate*.preferredName,\cr candidateList.candidate*.lastName,\cr candidateList.candidate*.suffix,\cr candidateList.candidate*.title,\cr candidateList.candidate*.ballotName,\cr candidateList.candidate*.electionParties,\cr candidateList.candidate*.electionStatus,\cr candidateList.candidate*.electionStage,\cr candidateList.candidate*.electionDistrictId,\cr candidateList.candidate*.electionDistrictName,\cr candidateList.candidate*.electionOffice,\cr candidateList.candidate*.electionOfficeId,\cr candidateList.candidate*.electionStateId,\cr candidateList.candidate*.electionOfficeTypeId,\cr candidateList.candidate*.electionYear,\cr candidateList.candidate*.electionSpecial,\cr candidateList.candidate*.electionDate,\cr candidateList.candidate*.officeParties,\cr candidateList.candidate*.officeStatus,\cr candidateList.candidate*.officeDistrictId,\cr candidateList.candidate*.officeDistrictName,\cr candidateList.candidate*.officeStateId,\cr candidateList.candidate*.officeId,\cr candidateList.candidate*.officeName,\cr candidateList.candidate*.officeTypeId,\cr candidateList.candidate*.runningMateId,\cr candidateList.candidate*.runningMateName.
##' @references http://api.votesmart.org/docs/Candidates.html\cr 
##' Use State.getStateIDs() to get a list of state IDs.\cr 
##' See http://api.votesmart.org/docs/semi-static.html for a list of office IDs or use Office.getOfficesByType(), Office.getOfficesByLevel(), Office.getOfficesByTypeLevel() or Office.getOfficesByBranchLevel() to get a list of office ID(s).\cr
##' See also: Matter U, Stutzer A (2015) pvsR: An Open Source Interface to Big Data on the American Political Sphere. PLoS ONE 10(7): e0130501. doi: 10.1371/journal.pone.0130501
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @examples
##' # First, make sure your personal PVS API key is saved as an option
##' # (options("pvs.key" = "yourkey")) or in the pvs.key variable:
##' \dontrun{pvs.key <- "yourkey"}
##' # get a data frame of candidates according to all state/office/electionYear combinations
##' \dontrun{candidates <- Candidates.getByOfficeState(stateId=list("NJ","NY"),officeId=list(6,7),
##' electionYear=list(2012,2008), all=TRUE)}
##' \dontrun{candidates}
##' # get a data frame of candidates according to the exact state/office/electionYear combinations 
##' # (i.e., "NY"/6/2012, "NJ"/7/2008)
##' \dontrun{candidates <- Candidates.getByOfficeState(stateId=list("NJ","NY"),officeId=list(6,7),
##'  electionYear=list(2012,2008), all=FALSE)}
##' \dontrun{candidates}
##' @export


Candidates.getByOfficeState <-
	function (stateId="NA", officeId, electionYear=NULL, all=FALSE) {
		if (!all) {
			nstateId <- length(stateId) 
			nofficeId <- length(officeId) 
			samelength <- nstateId == nofficeId
			
			if (!samelength) {
				stop(paste0("If all=FALSE, stateId and officeId must have the same length.\n",
							"stateId has length: ", nstateId, "\nofficeId has length: ", nofficeId))
			}
		}

		if (length(electionYear)==0) {
			# internal function
			Candidates.getByOfficeState.basic1 <- 
				function (.stateId, .officeId) {
					
					request <-  "Candidates.getByOfficeState?"
					inputs  <-  paste("&stateId=",.stateId,"&officeId=",.officeId,sep="")
					output  <-  pvsRequest4(request,inputs)
					output$stateId <- .stateId
					output$officeId <- .officeId
					return(output)
			}
			
			if (all==TRUE) {
				
				# Main function  
				output.list <- lapply(stateId, FUN= function (y) {
					lapply(officeId, FUN= function (s) {
						Candidates.getByOfficeState.basic1(.stateId=y, .officeId=s)
					}
					)
				}
				)
				
				
			} else {
				
				# Main function  
				reqdf <- data.frame(o=unlist(officeId), s=unlist(stateId), stringsAsFactors = FALSE)
				output.list <- lapply(1:dim(reqdf)[1], FUN= function (l) {
					Candidates.getByOfficeState.basic1(.stateId=reqdf[l,"s"], .officeId=reqdf[l,"o"])
				})
			}
			output.list <- redlist(output.list)
			
		} else {
			
			# internal function
			Candidates.getByOfficeState.basic2 <- 
				function (.stateId, .officeId, .electionYear) {
					
					request <-  "Candidates.getByOfficeState?"
					inputs  <-  paste("&stateId=",.stateId,"&officeId=",.officeId, "&electionYear=", .electionYear, sep="")
					output  <-  pvsRequest4(request,inputs)
					output$stateId <- .stateId
					output$officeId <- .officeId
					output$electionYear.input <- .electionYear
					
					return(output)
				}
			
			if (all==TRUE) {
				
				# Main function  
				output.list <- lapply(stateId, FUN= function (y) {
					lapply(officeId, FUN= function (s) {
						lapply(electionYear, FUN= function (c) {
							Candidates.getByOfficeState.basic2(.stateId=y, .officeId=s, .electionYear=c)
						}
						)
					}
					)
				}
				)
				
			} else {
				
				# Main function  
				reqdf <- data.frame(y=unlist(electionYear), s=unlist(stateId), o=unlist(officeId), stringsAsFactors = FALSE)
				output.list <- lapply(1:dim(reqdf)[1], FUN= function (l) {
					Candidates.getByOfficeState.basic2(.electionYear=reqdf[l,"y"], .stateId=reqdf[l,"s"], .officeId=reqdf[l,"o"])
				})
			}
			output.list <- redlist(output.list)
		}
		
		output <- bind_rows(output.list)

		# Avoids that output is missleading, because electionYear is already given in request-output, but also a
		# additionally generated (as electionYear.input). Problem exists because some request-outputs might be empty
		# and therefore only contain one "electionYear" whereas the non-empty ones contain two. (see basic function)
		if (!is.null(output$electionYear)) {
			na_entries <- as.vector(is.na(output$electionYear))
			output$electionYear[na_entries] <- output$electionYear.input[na_entries]
			output$electionYear.input <- NULL
		}
		 
		return(output)
	}

