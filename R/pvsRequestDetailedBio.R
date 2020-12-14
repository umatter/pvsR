pvsRequestDetailedBio <-
	function (request,inputs) {
		pvs.url <- paste("http://api.votesmart.org/",request,"key=",get('pvs.key',envir=.GlobalEnv),inputs,sep="") #generate url for request
		
		httpresp <- GET(url=pvs.url)
		xmltext <- content(x=httpresp, as="text")
		errors <-  getXMLErrors(xmltext) # check if xml can be parsed properly
		
		if (length(errors) != 0) {
			if (names(errors[[1]]$code) == "XML_ERR_CDATA_NOT_FINISHED") { # if not, try to fix 
				xmltext <- gsub(pattern="\003", replacement="", x=xmltext, fixed=TRUE)
			}
		}
		
		output.base <- xmlRoot(xmlTreeParse(xmltext, useInternalNodes=TRUE))
		
		if (names(output.base)[1]=="errorMessage") { # return empty df
			
			# if the requested data is not available, return an empty (NA) data frame and give a warning
			warning(gsub(pattern="&", replacement=" ", x=paste("No data available for: ", inputs,". The corresponding rows in the data frame are filled with NAs.", sep=""), fixed=TRUE), call.=FALSE)
			output.df <- data.frame(matrix(nrow=1,ncol=0), stringsAsFactors = FALSE)
			return(output.df)
			
		} else {
			output <- output.base 
			nodenames <- names(output) # get names of nodes
			
			# remove generally unnecessary child-nodes:
			if (nodenames[1]=="generalInfo") {
				output <- removeChildren(output,kids="generalInfo")
			} 
			
			# process detailed bio part separately
			separate <- c("education", "profession", "political", "congMembership", "orgMembership")
			candidate <- output[["candidate"]]
			
			sepl <- names(candidate) %in% separate
			
			# extract values for each remaining mainnode
			
			seplist <- lapply(separate, function(s) {
				data.frame(t(xmlSApply(candidate[[s]], function(x) xmlSApply(x, xmlValue))),row.names=NULL, stringsAsFactors = FALSE)
			})
			
			# give each list entry the coresponding name
			for (i in 1:length(seplist)) names(seplist)[i] <- separate[i]
			
			
			# ------- Second, the remaining nodes ---------
			
			# remove subnodes to be processed separately from main document
			output[["candidate"]] <- removeChildren(output[["candidate"]], kids=separate)
			# if office information available, process separately
			if ("office" %in% names(output)){
			  office <- output[["office"]]
			  office_df <- as.data.frame(t(xmlSApply(office, function(x) xmlValue(x))), stringsAsFactors = FALSE) 
			  output <- removeChildren(output, kids="office")
			  seplist$office <- office_df
			}
			
			# process rest as usual: 
			if (length(names(output))>1) {
				nonsep <- xmlSApply(output, function(x) data.frame(t(xmlSApply(x, xmlValue)), stringsAsFactors = FALSE))
			} else {
				nonsep <- list(data.frame(t(xmlSApply(output[[1]], xmlValue)), stringsAsFactors = FALSE))
				names(nonsep) <- names(output)
			}
			
			# return dfs in list
			output.list <- c(nonsep,seplist)
			
			# add empty dfs if some parts are missing
			dfn <- names(output.list)
			
			pseudo.output <- list(
				candidate=data.frame(candidateId="NA", stringsAsFactors = FALSE),
				office=data.frame(name="NA", stringsAsFactors = FALSE),
				education=data.frame(degree="NA", stringsAsFactors = FALSE),
				profession=data.frame(title="NA", stringsAsFactors = FALSE),
				political=data.frame(title="NA", stringsAsFactors = FALSE),
				congMembership=data.frame(title="NA", stringsAsFactors = FALSE),
				orgMembership=data.frame(title="NA", stringsAsFactors = FALSE),
				office=data.frame(title="NA", stringsAsFactors = FALSE)
			)
			nitems <- 1:length(pseudo.output)
			missing <- !(names(pseudo.output) %in% dfn) # missing nodes
			
			output.list <- c(output.list, pseudo.output[ nitems[missing]] )
			output.list <- output.list[names(pseudo.output)]
			
			empty <- lapply(output.list,ncol)==0 # empty nodes
			output.list[empty] <- pseudo.output[empty]
			output.list <- output.list[!duplicated(names(output.list))]
			
			return(output.list)
		}
	}
