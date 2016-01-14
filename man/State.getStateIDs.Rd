\name{State.getStateIDs}
\alias{State.getStateIDs}
\title{Get a list of states and their IDs.}
\usage{
State.getStateIDs()
}
\value{
A data frame with a row for each state:\cr
statelist.list.state*.stateId,\cr
statelist.list.state*.name
}
\description{
This function is a wrapper for the State.getStateIDs()
method of the PVS API State class which returns a simple
state ID and name list for mapping IDs to state names.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get a list of states with their IDs
\dontrun{stateIDs <- State.getStateIDs()}
\dontrun{stateIDs}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/State.html
}

