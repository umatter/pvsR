% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Candidates.getByElection.R
\name{Candidates.getByElection}
\alias{Candidates.getByElection}
\title{Get a list of candidates according to the election they are running for
 
This function is a wrapper for the Candidates.getByElection() method of the PVS API Candidates class which grabs a list of candidates according to the election they are running for. The function sends a request with this method to the PVS API for all electionIDs and stageIDs given as a function input, extracts the XML values from the returned XML file(s) and returns them arranged in one data frame.}
\usage{
Candidates.getByElection(electionId, stageId=NULL)
}
\arguments{
\item{electionId}{a character string or list of character strings with the election ID(s) (see references for details)}

\item{stageId}{(optional) a character string or list of character strings with the stage ID(s) (default: all)}
}
\value{
A data frame with a row for each candidate and columns with the following variables describing the candidate:\cr candidateList.candidate*.candidateId,\cr candidateList.candidate*.firstName,\cr candidateList.candidate*.nickName,\cr candidateList.candidate*.middleName,\cr candidateList.candidate*.preferredName,\cr candidateList.candidate*.lastName,\cr candidateList.candidate*.suffix,\cr candidateList.candidate*.title,\cr candidateList.candidate*.ballotName,\cr candidateList.candidate*.electionParties,\cr candidateList.candidate*.electionStatus,\cr candidateList.candidate*.electionStage,\cr candidateList.candidate*.electionDistrictId,\cr candidateList.candidate*.electionDistrictName,\cr candidateList.candidate*.electionOffice,\cr candidateList.candidate*.electionOfficeId,\cr candidateList.candidate*.electionStateId,\cr candidateList.candidate*.electionOfficeTypeId,\cr candidateList.candidate*.electionYear,\cr candidateList.candidate*.electionSpecial,\cr candidateList.candidate*.electionDate,\cr candidateList.candidate*.officeParties,\cr candidateList.candidate*.officeStatus,\cr candidateList.candidate*.officeDistrictId,\cr candidateList.candidate*.officeDistrictName,\cr candidateList.candidate*.officeStateId,\cr candidateList.candidate*.officeId,\cr candidateList.candidate*.officeName,\cr candidateList.candidate*.officeTypeId,\cr candidateList.candidate*.runningMateId,\cr candidateList.candidate*.runningMateName
}
\description{
Get a list of candidates according to the election they are running for
 
This function is a wrapper for the Candidates.getByElection() method of the PVS API Candidates class which grabs a list of candidates according to the election they are running for. The function sends a request with this method to the PVS API for all electionIDs and stageIDs given as a function input, extracts the XML values from the returned XML file(s) and returns them arranged in one data frame.
}
\examples{
# First, make sure your personal PVS API key is saved as an option
# (options("pvs.key" = "yourkey")) or in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get a list of candidates for certain election IDs
\dontrun{candidates <- Candidates.getByElection(list(2582,2646))}
\dontrun{candidates}
}
\references{
http://api.votesmart.org/docs/Candidates.html\cr 
Use Election.getElectionByYearState() or Election.getElectionByZip() to get election ID(s).\cr
See also: Matter U, Stutzer A (2015) pvsR: An Open Source Interface to Big Data on the American Political Sphere. PLoS ONE 10(7): e0130501. doi: 10.1371/journal.pone.0130501
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
