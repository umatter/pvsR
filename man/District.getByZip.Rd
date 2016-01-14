\name{District.getByZip}
\alias{District.getByZip}
\title{Get district IDs according to the zip code}
\usage{
District.getByZip(zip5, zip4=NULL)
}
\arguments{
  \item{zip5}{a character string or list of character
  strings with the five-digit ZIP code}

  \item{zip4}{(optional) a character string or list of
  character strings with the expanded ZIP+4 code (default:
  All)}
}
\value{
A data frame with a row for each district and columns with
the following variables describing the district:\cr
districtList.district*.districtId,\cr
districtList.district*.name,\cr
districtList.district*.officeId,\cr
districtList.district*.stateId.
}
\description{
This function is a wrapper for the District.getByZip()
method of the PVS API District class which grabs district
IDs according to the ZIP code. The function sends a request
with this method to the PVS API for all ZIP codes given as
a function input, extracts the XML values from the returned
XML file(s) and returns them arranged in one data frame.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get district IDs by ZIP Code
\dontrun{district <- District.getByZip(list(10001,10002),)}
\dontrun{district}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/District.html
}

