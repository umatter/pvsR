\name{Measure.getMeasuresByYearState}
\alias{Measure.getMeasuresByYearState}
\title{Get a list of state ballot measures in a given year}
\usage{
Measure.getMeasuresByYearState(year, stateId)
}
\arguments{
  \item{year}{a character string or list of character
  strings with the year(s)}

  \item{stateId}{a character string or list of character
  strings with the state ID(s) (see references for
  details)}
}
\value{
A data frame with a row for each ballot measure and columns
with the following variables describing the ballot
measure:\cr measures.measure*.measureId,\cr
measures.measure*.measureCode,\cr
measures.measure*.title,\cr measures.measure*.outcome.
}
\description{
This function is a wrapper for the
Measure.getMeasuresByYearState() method of the PVS API
Measure class which grabs a list of state ballot measures
in a given year. The function sends a request with this
method to the PVS API for all years and state IDs given as
a function input, extracts the XML values from the returned
XML file(s) and returns them arranged in one data frame.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get a list of ballot measures for a certain state and year
\dontrun{measures <- Measure.getMeasuresByYearState(list(2010,2012),"FL")}
\dontrun{measures}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.v.otesmart.org/docs/Measure.html\cr Use
State.getStateIDs() to get a list of state IDs.
}

