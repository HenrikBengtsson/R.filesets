%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{writeChecksum.GenericDataFile}
\alias{writeChecksum.GenericDataFile}
\alias{GenericDataFile.writeChecksum}
\alias{writeChecksum,GenericDataFile-method}

\title{Write the file checksum to a checksum file}

\description{
  Write the file checksum to a checksum file having the same filename with suffix \code{.md5} added.
}

\usage{
\method{writeChecksum}{GenericDataFile}(this, ..., skip=FALSE, verbose=FALSE)
}

\arguments{
 \item{skip}{If \code{\link[base:logical]{TRUE}}, an already written checksum file is skipped.}
 \item{...}{Not used.}
 \item{verbose}{...}
}

\value{
  Returns (invisibly) the pathname to the checksum file.
  An exception is thrown if the file does not exist.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  \code{\link[R.filesets:validateChecksum.GenericDataFile]{*validateChecksum}()}.
  \code{\link[R.filesets:compareChecksum.GenericDataFile]{*compareChecksum}()}.
  \code{\link[R.filesets:readChecksum.GenericDataFile]{*readChecksum}()}.
  \code{\link[R.filesets:getChecksum.GenericDataFile]{*getChecksum}()}.
  For more information see \code{\link{GenericDataFile}}.
}
\keyword{internal}
\keyword{methods}
