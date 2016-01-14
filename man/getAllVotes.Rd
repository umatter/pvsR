\name{getAllVotes}
\alias{getAllVotes}
\title{Get several votes}
\usage{
getAllVotes(actionId, batchsize=100, pause=0, backupfile="votes.list.Rdata")
}
\arguments{
  \item{actionId}{a character string or list of character
  strings with the action ID(s) (see references for
  details)}

  \item{batchsize}{numerical, indicating how many actionIds
  should be processed in one batch (defaults to 100).}

  \item{pause}{numerical, indicating how long (in seconds)
  the download process should be paused after each batch
  (defaults to 0)}

  \item{backupfile}{character string for the path/file-name
  of the Rdata-file where the data should be saved
  (batch-wise) during the download process (default:
  "votes.list.Rdata").}
}
\value{
A data frame with a row for each vote and columns with the
following variables describing the vote:\cr
votes.vote*.candidateId,\cr votes.vote*.candidateName,\cr
votes.vote*.officeParties,\cr votes.vote*.action.
}
\description{
This function is essentially a wrapper around
Votes.getBillActionVotes() specified for large amount of
requests.
}
\details{
This functions splits large requests into several batches.
The requests are then processed batch-wise and are saved on
the local disc to make sure that not too much RAM is
assigned to the pvsR task.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get all officials of a certain state
\dontrun{bill <- Votes.getBill("17623", separate=c("actions", "sponsors"))}
\dontrun{actionids <- bill$actions$actionId}
# get all votes on this acti
\dontrun{votes <- getAllVotes(actionids, batchsize=2)}
\dontrun{head(votes)}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/Votes.html\cr Use
Votes.getBill() or Votes.getByOfficial() to get a list of
action IDs.
}

