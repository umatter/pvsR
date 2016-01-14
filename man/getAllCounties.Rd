\name{getAllCounties}
\alias{getAllCounties}
\title{Get basic data on all counties}
\usage{
getAllCounties()
}
\value{
A data frame with a row for each county and columns with
the following variables describing the county:\cr
counties.county*.localId,\cr counties.county*.name,\cr
counties.county*.url.
}
\description{
This function is essentially a wrapper around
Local.getCounties().
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get a list of all counties
\dontrun{counties <- getAllCounties()}
\dontrun{head(counties)}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/Local.html\cr Use
State.getStateIDs() to get a list of state IDs.
}

