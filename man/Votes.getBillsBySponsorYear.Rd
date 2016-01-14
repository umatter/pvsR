\name{Votes.getBillsBySponsorYear}
\alias{Votes.getBillsBySponsorYear}
\title{Get a list of bills according to sponsor(candidate) and year}
\usage{
Votes.getBillsBySponsorYear(year, candidateId)
}
\arguments{
  \item{year}{a character string or list of character
  strings with the year(s)}

  \item{candidateId}{a character string or list of
  character strings with the candidate ID(s) (see
  references for details)}
}
\value{
A data frame with a row for each bill and columns with the
following variables describing the bill:\cr
bills.bill*.billId,\cr bills.bill*.billNumber,\cr
bills.bill*.title,\cr bills.bill*.type.
}
\description{
This function is a wrapper for the
Votes.getBillsBySponsorYear() method of the PVS API Votes
class which grabs a list of bills that fit the sponsor's
candidateId and year. The function sends a request with
this method to the PVS API for all candidate IDs and years
given as a function input, extracts the XML values from the
returned XML file(s) and returns them arranged in one data
frame.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get additional biographical data on Barak Obama
\dontrun{obama <- CandidateBio.getAddlBio(9490)}
\dontrun{obama}
# get additional biographical data on Barak Obama and Mitt Romney
\dontrun{onr <- CandidateBio.getAddlBio(list(9490,21942))}
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
Candidates.getByZip() to get a list of candidate IDs.
}

