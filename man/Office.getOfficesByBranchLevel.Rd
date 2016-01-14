\name{Office.getOfficesByBranchLevel}
\alias{Office.getOfficesByBranchLevel}
\title{Get offices tracked according to branch and level}
\usage{
Office.getOfficesByBranchLevel(branchId, levelId)
}
\arguments{
  \item{branchId}{a character string or list of character
  strings with the branch ID(s) (see references for
  details)}

  \item{levelId}{a character string or list of character
  strings with the level ID(s) (see references for
  details)}
}
\value{
A data frame with a row for each office and columns with
the following variables describing the office:\cr
offices.office*.officeId,\cr
offices.office*.officeTypeId,\cr
offices.office*.officeLevelId,\cr
offices.office*.officeBranchId,\cr offices.office*.name,\cr
offices.office*.title,\cr offices.office*.shortTitle.
}
\description{
This function is a wrapper for the
Office.getOfficesByBranchLevel() method of the PVS API
Office class which grabs a list of offices Project Vote
Smart keeps track of according to government branch and
level. The function sends a request with this method to the
PVS API for all branch and level IDs given as a function
input, extracts the XML values from the returned XML
file(s) and returns them arranged in one data frame.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get offices tracked for all branch and level IDs
\dontrun{offices <- Office.getOfficesByBranchLevel(list("E","L","J"),list("F","S","L"))}
\dontrun{head(offices)}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/Office.html\cr Use
Office.getBranches() to get branch ID(s).\cr Use
Office.getLevels() to get level ID(s).
}

