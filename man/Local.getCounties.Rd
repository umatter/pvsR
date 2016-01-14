\name{Local.getCounties}
\alias{Local.getCounties}
\title{Get counties in a state}
\usage{
Local.getCounties(stateId)
}
\arguments{
  \item{stateId}{a character string or list of character
  strings with the state ID(s) (see references for
  details)}
}
\value{
A data frame with a row for each county and columns with
the following variables describing the county:\cr
counties.county*.localId,\cr counties.county*.name,\cr
counties.county*.url.
}
\description{
This function is a wrapper for the Local.getCounties()
method of the PVS API Local class which returns a list of
counties in a state. The function sends a request with this
method to the PVS API for all state IDs given as a function
input, extracts the XML values from the returned XML
file(s) and returns them arranged in one data frame.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get a list of counties of a certain state
\dontrun{counties <- Local.getCounties(list("NY","FL"))}
\dontrun{head(counties)}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/Local.html\cr Use
State.getStateIDs() to get a list of state IDs.
}

