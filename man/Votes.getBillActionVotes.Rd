% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Votes.getBillActionVotes.R
\name{Votes.getBillActionVotes}
\alias{Votes.getBillActionVotes}
\title{Get votes listed by candidate on a certain bill action}
\usage{
Votes.getBillActionVotes(actionId)
}
\arguments{
\item{actionId}{a character string or list of character strings with the action ID(s) (see references for details)}
}
\value{
A data frame with a row for each vote and columns with the following variables describing the vote:\cr votes.vote*.candidateId,\cr votes.vote*.candidateName,\cr votes.vote*.officeParties,\cr votes.vote*.action.
}
\description{
This function is a wrapper for the Votes.getBillActionVotes() method of the PVS API Votes class which provides votes listed by candidate on a certain bill action. The function sends a request with this method to the PVS API for all action IDs given as a function input, extracts the XML values from the returned XML file(s) and returns them arranged in one data frame.
}
\examples{
# First, make sure your personal PVS API key is saved as an option
# (options("pvs.key" = "yourkey")) or in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get votes of a certain candidate on a certain action
\dontrun{actionvote <- Votes.getBillActionVotes(list(31712,28686))}
\dontrun{actionvote}
}
\references{
http://api.votesmart.org/docs/Votes.html\cr
Use Votes.getBill() or Votes.getByOfficial() to get a list of action IDs.
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
