\name{Candidates.getByOfficeTypeState}
\alias{Candidates.getByOfficeTypeState}
\title{Get a list of candidates according to office type and state representation}
\usage{
Candidates.getByOfficeTypeState(stateId="NA", officeTypeId, electionYear=NULL, all=FALSE)
}
\arguments{
  \item{stateId}{(optional) a character string or list of
  character strings with the state ID(s) (default: "NA",
  for national) (see references for details)}

  \item{officeTypeId}{a character string or list of
  character strings with the office type ID(s) (see
  references for details)}

  \item{electionYear}{(optional) a character string or list
  of character strings with the election year(s) (default:
  >= current year)}

  \item{all}{a logical indicator; if TRUE data on all
  possible combinations of the input variables are
  returned, if FALSE (default) only the exact combinations
  of them (see example)}
}
\value{
A data frame with a row for each candidate and columns with
the following variables describing the candidate:\cr
candidateList.candidate*.candidateId,\cr
candidateList.candidate*.firstName,\cr
candidateList.candidate*.nickName,\cr
candidateList.candidate*.middleName,\cr
candidateList.candidate*.preferredName,\cr
candidateList.candidate*.lastName,\cr
candidateList.candidate*.suffix,\cr
candidateList.candidate*.title,\cr
candidateList.candidate*.ballotName,\cr
candidateList.candidate*.electionParties,\cr
candidateList.candidate*.electionStatus,\cr
candidateList.candidate*.electionStage,\cr
candidateList.candidate*.electionDistrictId,\cr
candidateList.candidate*.electionDistrictName,\cr
candidateList.candidate*.electionOffice,\cr
candidateList.candidate*.electionofficeTypeId,\cr
candidateList.candidate*.electionStateId,\cr
candidateList.candidate*.electionOfficeTypeId,\cr
candidateList.candidate*.electionYear,\cr
candidateList.candidate*.electionSpecial,\cr
candidateList.candidate*.electionDate,\cr
candidateList.candidate*.officeParties,\cr
candidateList.candidate*.officeStatus,\cr
candidateList.candidate*.officeDistrictId,\cr
candidateList.candidate*.officeDistrictName,\cr
candidateList.candidate*.officeStateId,\cr
candidateList.candidate*.officeTypeId,\cr
candidateList.candidate*.officeName,\cr
candidateList.candidate*.officeTypeId,\cr
candidateList.candidate*.runningMateId,\cr
candidateList.candidate*.runningMateName.
}
\description{
This function is a wrapper for the
Candidates.getByOfficeTypeState() method of the PVS API
Candidate class which grabs a list of candidates according
to office type and state representation. The function sends
a request with this method to the PVS API for all state
IDs, office type IDs and election years given as a function
input, extracts the XML values from the returned XML
file(s) and returns them arranged in one data frame.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get a data frame of candidates according to all state/officeType/electionYear combinations
\dontrun{candidates <- Candidates.getByOfficeTypeState(stateId=list("NJ","NY"),
officeTypeId=list("C","L"), electionYear=list(2012,2008), all=TRUE)}
\dontrun{candidates}
# get a data frame of candidates according to the exact state/officeType/electionYear combinations
# (i.e., "NY"/6/2012, "NJ"/7/2008)
\dontrun{candidates <- Candidates.getByOfficeTypeState(stateId=list("NJ","NY"),
officeTypeId=list("C","L"), electionYear=list(2012,2008), all=FALSE)}
\dontrun{candidates}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/Candidates.html\cr Use
State.getStateIDs() to get a list of state IDs.\cr See
http://api.votesmart.org/docs/semi-static.html or use
Office.getTypes() or Office.getOfficesByLevel() to get a
list of office types ID(s).
}

