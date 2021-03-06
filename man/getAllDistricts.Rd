% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getAllDistricts.R
\name{getAllDistricts}
\alias{getAllDistricts}
\title{Get basic data on all districts}
\usage{
getAllDistricts()
}
\value{
A data frame with a row for each district and columns with the following variables describing the district:\cr districtList.district*.districtId,\cr districtList.district*.name,\cr districtList.district*.officeId,\cr districtList.district*.stateId.
}
\description{
This function is essentially a wrapper around District.getByOfficeState().
}
\examples{
# First, make sure your personal PVS API key is saved as an option
# (options("pvs.key" = "yourkey")) or in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get a list of all districts
\dontrun{districts <- getAllDistricts()}
\dontrun{head(districts)}
}
\references{
http://api.votesmart.org/docs/District.html\cr
See http://api.votesmart.org/docs/semi-static.html for a list of office IDs or use Office.getOfficesByType(), Office.getOfficesByLevel(), Office.getOfficesByTypeLevel() or Office.getOfficesByBranchLevel().\cr
Use State.getStateIDs() to get a list of state IDs.\cr
See also: Matter U, Stutzer A (2015) pvsR: An Open Source Interface to Big Data on the American Political Sphere. PLoS ONE 10(7): e0130501. doi: 10.1371/journal.pone.0130501
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
