\name{Votes.getBillAction}
\alias{Votes.getBillAction}
\title{Get detailed action information on a certain stage of the bill}
\usage{
Votes.getBillAction(actionId)
}
\arguments{
  \item{actionId}{a character string or list of character
  strings with the action ID(s) (see references for
  details)}
}
\value{
A data frame with a row for each action and columns with
the following variables describing the action:\cr
action.billId,\cr action.billNumber,\cr action.actionId,\cr
action.category,\cr action.categoryId,\cr action.type,\cr
action.stateId,\cr action.level,\cr action.stage,\cr
action.outcome,\cr action.rollNumber,\cr action.yea,\cr
action.nay,\cr action.voiceVote,\cr action.title,\cr
action.officialTitle,\cr action.highlight,\cr
action.synopsis,\cr action.officialSynopsis,\cr
action.note.
}
\description{
This function is a wrapper for the Votes.getBillAction()
method of the PVS API Votes class which grabs detailed
action information on a certain stage of the bill. The
function sends a request with this method to the PVS API
for all action IDs given as a function input, extracts the
XML values from the returned XML file(s) and returns them
arranged in one data frame.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get information about certain actions
\dontrun{actioninfo <- Votes.getBillAction(actionId=list(2575,18436,10194))}
\dontrun{actioninfo}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/Votes.html\cr Use
Votes.getBill() or Votes.getByOfficial() to get a list of
action IDs.
}

