##' Get several candidates' detailed biographical information 
##' 
##' This function is essentially a  wrapper around CandidateBio.getDetailedBio() specified for large amount of requests.
##' @usage getAllBios(candidateId, batchsize=100, pause=0, backupfile="bios.list.Rdata")
##' @param candidateId a character string or list of character strings with the candidate ID(s) (see references for details)
##' @param batchsize numerical, indicating how many candidateIds should be processed in one batch (defaults to 100).
##' @param pause numerical, indicating how long (in seconds) the download process should be paused after each batch (defaults to 0)
##' @param backupfile character string for the path/file-name of the Rdata-file where the data should be saved (batch-wise) during the download process (default: "bios.list.Rdata").
##' @return A list with several data frames containing the elements of CandidateBio.getBio(), and expands upon:\cr bio.candidate.education.degree,\cr bio.candidate.education.field,\cr bio.candidate.education.school,\cr bio.candidate.education.span,\cr bio.candidate.education.gpa,\cr bio.candidate.education.fullText,\cr bio.candidate.profession.title,\cr bio.candidate.profession.organization,\cr bio.candidate.profession.span,\cr bio.candidate.profession.special,\cr bio.candidate.profession.district,\cr bio.candidate.profession.fullText,\cr bio.candidate.political.title,\cr bio.candidate.political.organization,\cr bio.candidate.political.span,\cr bio.candidate.political.special,\cr bio.candidate.political.district,\cr bio.candidate.political.fullText,\cr bio.candidate.congMembership.title,\cr bio.candidate.congMembership.organization,\cr bio.candidate.congMembership.span,\cr bio.candidate.congMembership.special,\cr bio.candidate.congMembership.district,\cr bio.candidate.congMembership.fullText,\cr bio.candidate.orgMembership.title,\cr bio.candidate.orgMembership.organization,\cr bio.candidate.orgMembership.span,\cr bio.candidate.orgMembership.special,\cr bio.candidate.orgMembership.district,\cr bio.candidate.orgMembership.fullText.
##' @details This functions splits large requests into several batches. The requests are then processed batch-wise and are saved on the local disc to make sure that not too much RAM is assigned to the pvsR task.
##' @references http://api.votesmart.org/docs/CandidateBio.html\cr 
##' Use Candidates.getByOfficeState(), Candidates.getByOfficeTypeState(), Candidates.getByLastname(), Candidates.getByLevenshtein(), Candidates.getByElection(), Candidates.getByDistrict() or Candidates.getByZip() to get a list of candidate IDs.\cr
##' See also: Matter U, Stutzer A (2015) pvsR: An Open Source Interface to Big Data on the American Political Sphere. PLoS ONE 10(7): e0130501. doi: 10.1371/journal.pone.0130501
##' @author Ulrich Matter <ulrich.matter-at-unibas.ch>
##' @examples
##' # First, make sure your personal PVS API key is saved as an option
##' # (options("pvs.key" = "yourkey")) or in the pvs.key variable:
##' \dontrun{pvs.key <- "yourkey"}
##' # get all officials of a certain state
##' \dontrun{officials <- Officials.getStatewide("FL")}
##' # get all biographical information on those officials
##' \dontrun{bios <- getAllDetailedBios(officials$candidateId[1:100], batchsize=20)}
##' \dontrun{str(bios)}
##' \dontrun{head(bios$education)}
##' @export
##' 


getAllDetailedBios <-
	function(candidateId, batchsize=100, pause=0, backupfile="biosdetails.list.Rdata") {

		n <- length(candidateId)
		if (n < batchsize) {
		  batchsize <- n
		}
		rest <- n%%batchsize
		chunks.upper <- seq(from = batchsize, to = n, by = batchsize)
		if (rest != 0) {
			chunks.upper[length(chunks.upper) + 1] <- chunks.upper[length(chunks.upper)] + rest
		}
		chunks.lower <- c(1,chunks.upper[-length(chunks.upper)] + 1)
	
		# prepare for loop over all chunks
		chunks <- data.frame(lower=chunks.lower, upper=chunks.upper)
		pb <- txtProgressBar(min = 0, max = nrow(chunks), style = 3)
		
		bios.list <- as.list(1:nrow(chunks))
		save(bios.list, file=backupfile) # to be saved and loaded in each loop
		
		# process queries chunkwise
		for (i in 1:nrow(chunks)) {
			
			Sys.sleep(pause)
			
			first <- chunks$lower[i]
			last <- chunks$upper[i]
			cIds <- candidateId[first:last]
			bios <- CandidateBio.getDetailedBio(cIds)
			
			load(backupfile)
			bios.list[[i]] <- bios
			save(bios.list, file=backupfile)
			rm(bios.list )
			gc(verbose=FALSE) # clean memory
			
			setTxtProgressBar(pb, i)
			
		}
		
		load(backupfile)
		
		# combine chunks to dfs per topic
		candidate <- bind_rows(lapply(bios.list, function(i) i$candidate))
		office <- bind_rows(lapply(bios.list, function(i) i$office))
		education <- bind_rows(lapply(bios.list, function(i) i$education))
		profession <- bind_rows(lapply(bios.list, function(i) i$profession))
		political <- bind_rows(lapply(bios.list, function(i) i$political))
		congMembership <- bind_rows(lapply(bios.list, function(i) i$congMembership))
		orgMembership <- bind_rows(lapply(bios.list, function(i) i$orgMembership))


		allBios <- list(candidate=candidate,
		                office=office,
		                education=education,
		                profession=profession,
		                political=political,
		                congMembership=congMembership,
		                orgMembership=orgMembership)

		return(allBios)
	}
  