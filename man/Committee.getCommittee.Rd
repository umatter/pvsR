\name{Committee.getCommittee}
\alias{Committee.getCommittee}
\title{Get detailed committee (contact) information}
\usage{
Committee.getCommittee(committeeId)
}
\arguments{
  \item{committeeId}{a character string or list of
  character strings with the committee ID(s) (see
  references for details)}
}
\value{
A data frame with a row for each committee and columns with
the following variables describing the committee:\cr
committee.committeeId,\cr committee.parentId,\cr
committee.stateId,\cr committee.committeeTypeId,\cr
committee.name,\cr committee.jurisdiction,\cr
committee.contact.address1,\cr
committee.contact.address2,\cr committee.contact.city,\cr
committee.contact.state,\cr committee.contact.zip,\cr
committee.contact.phone,\cr committee.contact.fax,\cr
committee.contact.email,\cr committee.contact.url,\cr
committee.contact.staffContact.
}
\description{
This function is a wrapper for the Committee.getCommittee()
method of the PVS API Committee class which returns
detailed committee data. The function sends a request with
this method to the PVS API for all committee IDs given as a
function input, extracts the XML values from the returned
XML file(s) and returns them arranged in one data frame.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get information about a certain committee
\dontrun{committee <- Committee.getCommittee(list(1,2))}
\dontrun{committee}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/Committee.html\cr Use
CandidateBio.getBio(), Committee.getCommitteesByTypeState()
or Votes.getBill() to get committee ID(s).
}

