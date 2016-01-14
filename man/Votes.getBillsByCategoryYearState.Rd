\name{Votes.getBillsByCategoryYearState}
\alias{Votes.getBillsByCategoryYearState}
\title{Get a list of bills according to category, year and state}
\usage{
Votes.getBillsByCategoryYearState(year, stateId, categoryId)
}
\arguments{
  \item{year}{a character string or list of character
  strings with the year ID(s)}

  \item{stateId}{a character string or list of character
  strings with the state ID(s) (see references for
  details)}

  \item{categoryId}{a character string or list of character
  strings with the category ID(s) (see references for
  details)}
}
\value{
A data frame with a row for each bill and columns with the
following variables describing the bill:\cr
bills.bill*.billId,\cr bills.bill*.billNumber,\cr
bills.bill*.title,\cr bills.bill*.type.
}
\description{
This function is a wrapper for the
Votes.getBillsByCategoryYearState() method of the PVS API
Votes class which grabs a list of bills according to
category, year and state. The function sends a request with
this method to the PVS API for all years, state and
category IDs given as a function input, extracts the XML
values from the returned XML file(s) and returns them
arranged in one data frame.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get a list of bills in a certain category, year and state
\dontrun{bills <- Votes.getBillsByCategoryYearState(as.list(2010:2012),"NY",10)}
\dontrun{bills}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/Votes.html\cr Use
State.getStateIDs() to get a list of state IDs.\cr Use
Votes.getCategories() or Rating.getCandidateRating() to get
a list of category IDs.
}

