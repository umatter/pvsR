\name{getAllDistricts}
\alias{getAllDistricts}
\title{Get basic data on all districts}
\usage{
getAllDistricts()
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
This function is essentially a wrapper around
District.getByOfficeState().
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get a list of all districts
\dontrun{districts <- getAllDistricts()}
\dontrun{head(districts)}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/District.html\cr See
http://api.votesmart.org/docs/semi-static.html for a list
of office IDs or use Office.getOfficesByType(),
Office.getOfficesByLevel(), Office.getOfficesByTypeLevel()
or Office.getOfficesByBranchLevel().\cr Use
State.getStateIDs() to get a list of state IDs.
}

