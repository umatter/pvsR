\name{Votes.getBillsByOfficialYearOffice}
\alias{Votes.getBillsByOfficialYearOffice}
\title{Get a list of bills according to office (optional), candidate and year}
\usage{
Votes.getBillsByOfficialYearOffice(year, candidateId, officeId=NULL)
}
\arguments{
  \item{year}{a character string or list of character
  strings with the year ID(s)}

  \item{candidateId}{a character string or list of
  character strings with the candidate ID(s) (see
  references for details)}

  \item{officeId}{(optional) a character string or list of
  character strings with the office ID(s) (default: all)
  (see references for details)}
}
\value{
A data frame with a row for each bill and columns with the
following variables describing the bill:\cr
bills.bill*.billId,\cr bills.bill*.billNumber,\cr
bills.bill*.title,\cr bills.bill*.type.
}
\description{
This function is a wrapper for the
Votes.getBillsByOfficialYearOffice() method of the PVS API
Votes class which grabs a list of bills that fit the
candidate and year. The function sends a request with this
method to the PVS API for all years, candidate and office
IDs given as a function input, extracts the XML values from
the returned XML file(s) and returns them arranged in one
data frame.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get a list of bills for a certain office, candidate and year
\dontrun{bills <- Votes.getBillsByOfficialYearOffice(list(2010,2011,2012),107800,)}
\dontrun{bills}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/Votes.html\cr Use
Candidates.getByOfficeState(),
Candidates.getByOfficeTypeState(),
Candidates.getByLastname(), Candidates.getByLevenshtein(),
Candidates.getByElection(), Candidates.getByDistrict() or
Candidates.getByZip() to get a list of candidate IDs.\cr
See http://api.votesmart.org/docs/semi-static.html for a
list of office IDs or use Office.getOfficesByType(),
Office.getOfficesByLevel(), Office.getOfficesByTypeLevel()
or Office.getOfficesByBranchLevel
}

