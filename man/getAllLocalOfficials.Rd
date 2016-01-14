\name{getAllLocalOfficials}
\alias{getAllLocalOfficials}
\title{Fetch data on all local (city- or county-) officials}
\usage{
getAllLocalOfficials(locality="counties", batchsize=50,
 pause=0, backupfile="locofs.list.Rdata")
}
\arguments{
  \item{locality}{a character string indicating whether
  data on county-officials ("counties") or city-officials
  ("cities") should be downloaded.}

  \item{batchsize}{numerical, indicating the number of
  requests that should be processed in one batch (defaults
  to 50).}

  \item{pause}{numerical, indicating how long (in seconds)
  the download process should be paused after each batch
  (defaults to 0)}

  \item{backupfile}{character string for the path/file-name
  of the Rdata-file where the data should be saved
  (batch-wise) during the download process (default:
  "locofs.list.Rdata").}
}
\value{
A data frame with a row for each official and columns with
the following variables describing the official:\cr
candidatelist.candidate*.candidateId,\cr
candidatelist.candidate*.firstName,\cr
candidatelist.candidate*.nickName,\cr
candidatelist.candidate*.middleName,\cr
candidatelist.candidate*.lastName,\cr
candidatelist.candidate*.suffix,\cr
candidatelist.candidate*.title,\cr
candidatelist.candidate*.electionParties,\cr
candidatelist.candidate*.electionDistrictId,\cr
candidatelist.candidate*.electionStateId,\cr
candidatelist.candidate*.officeParties,\cr
candidatelist.candidate*.officeDistrictId,\cr
candidatelist.candidate*.officeDistrictName,\cr
candidatelist.candidate*.officeStateId,\cr
candidatelist.candidate*.officeId,\cr
candidatelist.candidate*.officeName,\cr
candidatelist.candidate*.officeTypeId.
}
\description{
This function is essentially a wrapper around
Local.getOfficials().
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
# get a list of all local officials
\dontrun{all_countyofficials <- getAllLocalOfficials()}
\dontrun{head(officials)}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/Local.html\cr Use
Local.getCounties() or Local.getCities() to get a list of
local IDs.
}

