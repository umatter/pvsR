% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Leadership.getOfficials.R
\name{Leadership.getOfficials}
\alias{Leadership.getOfficials}
\title{Get officials that hold the leadership role in certain states}
\usage{
Leadership.getOfficials(stateId="NA", leadershipId)
}
\arguments{
\item{stateId}{(optional) a character string or list of character strings with the state ID(s) (default: "NA", for national) (see references for details)}

\item{leadershipId}{a character string or list of character strings with the leadership ID(s) (see references for details)}
}
\value{
A data frame with a row for each leadership position and columns with the following variables describing the official:\cr leaders.leader*.candidateId,\cr leaders.leader*.firstName,\cr leaders.leader*.middleName,\cr leaders.leader*.lastName,\cr leaders.leader*.suffix,\cr leaders.leader*.position,\cr leaders.leader*.officeId,\cr leaders.leader*.title.
}
\description{
This function is a wrapper for the Leadership.getOfficials() method of the PVS API Leadership class which grabs a list of officials that hold the leadership role in certain states. The function sends a request with this method to the PVS API for all state and leadership IDs given as a function input, extracts the XML values from the returned XML file(s) and returns them arranged in one data frame.
}
\examples{
# First, make sure your personal PVS API key is saved as an option
# (options("pvs.key" = "yourkey")) or in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get leaders by state ID and leadership ID 
\dontrun{officials <- Leadership.getOfficials(list("NY","FL"),list(138,140))}
\dontrun{officials}
}
\references{
http://api.votesmart.org/docs/Leadership.html\cr
Use State.getStateIDs() to get a list of state IDs.\cr
Use Leadership.getPositions() to get a list of leadership IDs.\cr
See also: Matter U, Stutzer A (2015) pvsR: An Open Source Interface to Big Data on the American Political Sphere. PLoS ONE 10(7): e0130501. doi: 10.1371/journal.pone.0130501
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
