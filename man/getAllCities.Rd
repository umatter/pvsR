\name{getAllCities}
\alias{getAllCities}
\title{Get basic data on all cities}
\usage{
getAllCities()
}
\value{
A data frame with a row for each city and columns with the
following variables describing the city:\cr
cities.city*.localId,\cr cities.city*.name,\cr
cities.city*.url.
}
\description{
This function is essentially a wrapper around
Local.getCities().
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get a list of all cities
\dontrun{cities <- getAllCities()}
\dontrun{head(cities)}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/Local.html\cr Use
State.getStateIDs() to get a list of state IDs.
}

