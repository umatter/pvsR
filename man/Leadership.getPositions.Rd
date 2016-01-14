\name{Leadership.getPositions}
\alias{Leadership.getPositions}
\title{Get leadership positions by state and office}
\usage{
Leadership.getPositions(stateId="NA", officeId=NULL)
}
\arguments{
  \item{stateId}{(optional) a character string or list of
  character strings with the state ID(s) (default: "NA",
  for national) (see references for details)}

  \item{officeId}{(optional) a character string or list of
  character strings with the office ID(s) (default: All)
  (see references for details)}
}
\value{
A data frame with a row for each leadership position and
columns with the following variables describing the
position:\cr leadership.position*.leadershipId,\cr
leadership.position*.name,\cr
leadership.position*.officeId,\cr
leadership.position*.officeName.
}
\description{
This function is a wrapper for the
Leadership.getPositions() method of the PVS API Leadership
class which returns leadership positions by state and
office. The function sends a request with this method to
the PVS API for all state and office IDs given as a
function input, extracts the XML values from the returned
XML file(s) and returns them arranged in one data frame.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get leadership positions by state ID and office ID
\dontrun{positions <- Leadership.getPositions(list("AL","FL"),8)}
\dontrun{positions}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/Leadership.html\cr Use
State.getStateIDs() to get a list of state IDs.\cr See
http://api.votesmart.org/docs/semi-static.html for a list
of office IDs or use Office.getOfficesByType(),
Office.getOfficesByLevel(), Office.getOfficesByTypeLevel()
or Office.getOfficesByBranchLevel() to get a list of office
ID(s).
}

