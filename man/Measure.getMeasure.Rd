\name{Measure.getMeasure}
\alias{Measure.getMeasure}
\title{Get details of a ballot measure}
\usage{
Measure.getMeasure(measureId)
}
\arguments{
  \item{measureId}{a character string or list of character
  strings with the measure ID(s) (see references for
  details)}
}
\value{
A data frame with a row for each ballot measure and columns
with the following variables describing the ballot
measure:\cr measure.measureId,\cr measure.measureCode,\cr
measure.title,\cr measure.electionDate,\cr
measure.electionType,\cr measure.source,\cr measure.url,\cr
measure.summary,\cr measure.summaryUrl,\cr
measure.measureText,\cr measure.textUrl,\cr
measure.proUrl,\cr measure.conUrl,\cr measure.yes,\cr
measure.no,\cr measure.outcome.
}
\description{
This function is a wrapper for the Measure.getMeasure()
method of the PVS API Measure class which grabs details of
a ballot measure. The function sends a request with this
method to the PVS API for all measure IDs given as a
function input, extracts the XML values from the returned
XML file(s) and returns them arranged in one data frame.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get details on certain ballot measures
\dontrun{measure <- Measure.getMeasure(list(1632,1633))}
\dontrun{measure}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/Measure.html\cr Use
Measure.getMeasuresByYearState() to get a list of measure
IDs.
}

